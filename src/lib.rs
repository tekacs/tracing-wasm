use core::fmt::{self, Write};
use core::sync::atomic::AtomicUsize;


use tracing::{
    dispatcher::SetGlobalDefaultError,
    field::{Field, Visit},
};
use tracing::Subscriber;
use tracing_subscriber::layer::*;
use tracing_subscriber::registry::*;
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
extern "C" {
    #[wasm_bindgen(js_namespace = performance)]
    fn mark(a: &str);
    #[wasm_bindgen(catch, js_namespace = performance)]
    fn measure(name: String, startMark: String) -> Result<(), JsValue>;
}

#[derive(Debug, PartialEq)]
pub struct WASMLayerConfig {
    pub report_logs_in_timings: bool,
    pub report_logs_in_console: bool,
    pub max_level: tracing::Level,
}

impl Default for WASMLayerConfig {
    fn default() -> Self {
        WASMLayerConfig {
            report_logs_in_timings: true,
            report_logs_in_console: true,
            max_level: tracing::Level::TRACE,
        }
    }
}

/// Implements [Layer] which uses [wasm_bindgen] for marking and measuring with `window.performance`
pub struct WASMLayer {
    last_event_id: AtomicUsize,
    config: WASMLayerConfig,
}

impl WASMLayer {
    pub fn new(config: WASMLayerConfig) -> Self {
        WASMLayer {
            last_event_id: AtomicUsize::new(0),
            config,
        }
    }
}

impl Default for WASMLayer {
    fn default() -> Self {
        WASMLayer::new(WASMLayerConfig::default())
    }
}

fn mark_name(id: &tracing::Id) -> String {
    format!("t{:x}", id.into_u64())
}

fn trace_level_to_log_level(trace_level: &tracing::Level) -> log::Level {
    match *trace_level {
        tracing::Level::TRACE => log::Level::Trace,
        tracing::Level::DEBUG => log::Level::Debug,
        tracing::Level::INFO => log::Level::Info,
        tracing::Level::WARN => log::Level::Warn,
        tracing::Level::ERROR => log::Level::Error,
    }
}

impl<S: Subscriber + for<'a> LookupSpan<'a>> Layer<S> for WASMLayer {
    fn enabled(&self, metadata: &tracing::Metadata<'_>, _: Context<'_, S>) -> bool {
        let level = metadata.level();
        level <= &self.config.max_level
    }

    fn on_new_span(
        &self,
        attrs: &tracing::span::Attributes<'_>,
        id: &tracing::Id,
        ctx: Context<'_, S>,
    ) {
        let mut new_debug_record = StringRecorder::new();
        attrs.record(&mut new_debug_record);

        if let Some(span_ref) = ctx.span(id) {
            span_ref
                .extensions_mut()
                .insert::<StringRecorder>(new_debug_record);
        }

        if self.config.report_logs_in_console {
            let meta = attrs.metadata();
            let level = meta.level();
            let target = if attrs.is_empty() {
                "tracing::span"
            } else {
                meta.target()
            };
            let body = format!("{}", tracing_logger::LogValueSet { values: attrs.values(), is_first: false });
            let log_level = trace_level_to_log_level(level);
            log::logger().log(
                &log::RecordBuilder::new()
                    .module_path(Some(meta.module_path().unwrap_or_default()))
                    .target(target)
                    .file(Some(meta.file().unwrap_or_default()))
                    .line(Some(meta.line().unwrap_or_default()))
                    .level(log_level)
                    .args(format_args!("{};{}", meta.name(), body))
                    .build()
            )
        }
    }

    /// doc: Notifies this layer that a span with the given Id recorded the given values.
    fn on_record(&self, id: &tracing::Id, values: &tracing::span::Record<'_>, ctx: Context<'_, S>) {
        if let Some(span_ref) = ctx.span(id) {
            if let Some(debug_record) = span_ref.extensions_mut().get_mut::<StringRecorder>() {
                values.record(debug_record);
            }
        }
    }

    // /// doc: Notifies this layer that a span with the ID span recorded that it follows from the span with the ID follows.
    // fn on_follows_from(&self, _span: &tracing::Id, _follows: &tracing::Id, ctx: Context<'_, S>) {}
    /// doc: Notifies this layer that an event has occurred.
    fn on_event(&self, event: &tracing::Event<'_>, _ctx: Context<'_, S>) {
        if self.config.report_logs_in_timings || self.config.report_logs_in_console {
            let mut recorder = StringRecorder::new();
            event.record(&mut recorder);
            let meta = event.metadata();
            let level = meta.level();
            if self.config.report_logs_in_console {
                let log_level = trace_level_to_log_level(level);
                log::logger().log(
                    &log::RecordBuilder::new()
                        .module_path(Some(meta.module_path().unwrap_or_default()))
                        .target(meta.target())
                        .file(Some(meta.file().unwrap_or_default()))
                        .line(Some(meta.line().unwrap_or_default()))
                        .level(log_level)
                        .args(format_args!("{}", recorder))
                        .build()
                )
            }
            if self.config.report_logs_in_timings {
                let mark_name = format!(
                    "c{:x}",
                    self.last_event_id
                        .fetch_add(1, core::sync::atomic::Ordering::Relaxed)
                );
                // mark and measure so you can see a little blip in the profile
                mark(&mark_name);
                let _ = measure(
                    format!(
                        "{} {} {}",
                        level,
                        meta.module_path().unwrap_or("..."),
                        recorder,
                    ),
                    mark_name,
                );
            }
        }
    }
    /// doc: Notifies this layer that a span with the given ID was entered.
    fn on_enter(&self, id: &tracing::Id, _ctx: Context<'_, S>) {
        mark(&mark_name(id));
    }
    /// doc: Notifies this layer that the span with the given ID was exited.
    fn on_exit(&self, id: &tracing::Id, ctx: Context<'_, S>) {
        if let Some(span_ref) = ctx.span(id) {
            let meta = span_ref.metadata();
            if let Some(debug_record) = span_ref.extensions().get::<StringRecorder>() {
                let _ = measure(
                    format!(
                        "\"{}\" {} {}",
                        meta.name(),
                        meta.module_path().unwrap_or("..."),
                        debug_record,
                    ),
                    mark_name(id),
                );
            } else {
                let _ = measure(
                    format!(
                        "\"{}\" {}",
                        meta.name(),
                        meta.module_path().unwrap_or("..."),
                    ),
                    mark_name(id),
                );
            }
        }
    }
    // /// doc: Notifies this layer that the span with the given ID has been closed.
    // /// We can dispose of any data for the span we might have here...
    // fn on_close(&self, _id: tracing::Id, ctx: Context<'_, S>) {}
    // /// doc: Notifies this layer that a span ID has been cloned, and that the subscriber returned a different ID.
    // /// I'm not sure if I need to do something here...
    // fn on_id_change(&self, _old: &tracing::Id, _new: &tracing::Id, ctx: Context<'_, S>) {}
}

/// Set the global default with [tracing::subscriber::set_global_default]
pub fn set_as_global_default() {
    tracing::subscriber::set_global_default(
        Registry::default().with(WASMLayer::new(WASMLayerConfig::default())),
    )
        .expect("default global");
}

/// Set the global default with [tracing::subscriber::set_global_default]
pub fn try_set_as_global_default() -> Result<(), SetGlobalDefaultError> {
    tracing::subscriber::set_global_default(
        Registry::default().with(WASMLayer::new(WASMLayerConfig::default())),
    )
}

/// Set the global default with [tracing::subscriber::set_global_default]
pub fn set_as_global_default_with_config(config: WASMLayerConfig) {
    tracing::subscriber::set_global_default(Registry::default().with(WASMLayer::new(config)))
        .expect("default global");
}

struct StringRecorder {
    display: String,
    is_following_args: bool,
}

impl StringRecorder {
    fn new() -> Self {
        StringRecorder {
            display: String::new(),
            is_following_args: false,
        }
    }
}

impl Visit for StringRecorder {
    fn record_debug(&mut self, field: &Field, value: &dyn fmt::Debug) {
        if field.name() == "message" {
            if !self.display.is_empty() {
                self.display = format!("{:?}\n{}", value, self.display)
            } else {
                self.display = format!("{:?}", value)
            }
        } else {
            if self.is_following_args {
                // following args
                writeln!(self.display).unwrap();
            } else {
                // first arg
                write!(self.display, " ").unwrap();
                self.is_following_args = true;
            }
            write!(self.display, "{} = {:?};", field.name(), value).unwrap();
        }
    }
}

impl fmt::Display for StringRecorder {
    fn fmt(&self, mut f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        if !self.display.is_empty() {
            write!(&mut f, " {}", self.display)
        } else {
            Ok(())
        }
    }
}

impl Default for StringRecorder {
    fn default() -> Self {
        StringRecorder::new()
    }
}

pub mod tracing_logger {
    use core::fmt;

    pub use log::*;
    use tracing::field::{Field, ValueSet, Visit};

    /// Utility to format [`ValueSet`]s for logging.
    pub(crate) struct LogValueSet<'a> {
        pub(crate) values: &'a ValueSet<'a>,
        pub(crate) is_first: bool,
    }

    impl<'a> fmt::Display for LogValueSet<'a> {
        #[inline]
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            struct LogVisitor<'a, 'b> {
                f: &'a mut fmt::Formatter<'b>,
                is_first: bool,
                result: fmt::Result,
            }

            impl Visit for LogVisitor<'_, '_> {
                fn record_str(&mut self, field: &Field, value: &str) {
                    if field.name() == "message" {
                        self.record_debug(field, &format_args!("{}", value))
                    } else {
                        self.record_debug(field, &value)
                    }
                }

                fn record_debug(&mut self, field: &Field, value: &dyn fmt::Debug) {
                    let res = if self.is_first {
                        self.is_first = false;
                        if field.name() == "message" {
                            write!(self.f, "{:?}", value)
                        } else {
                            write!(self.f, "{}={:?}", field.name(), value)
                        }
                    } else {
                        write!(self.f, " {}={:?}", field.name(), value)
                    };
                    if let Err(err) = res {
                        self.result = self.result.and(Err(err));
                    }
                }
            }

            let mut visit = LogVisitor {
                f,
                is_first: self.is_first,
                result: Ok(()),
            };
            self.values.record(&mut visit);
            visit.result
        }
    }
}

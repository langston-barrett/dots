fn init_tracing(level: Level) {
    let builder = tracing_subscriber::fmt::fmt()
        .with_target(false)
        .with_max_level(level)
        .with_writer(std::io::stderr);
    if let Level::TRACE = level {
        let builder = builder.with_span_events(FmtSpan::ENTER | FmtSpan::CLOSE);
        builder.init();
    } else {
        let builder = builder.without_time();
        builder.init();
    }
}

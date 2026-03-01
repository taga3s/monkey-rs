#[derive(PartialEq, Debug)]
pub struct Context<'ctx> {
    pub source: &'ctx str,
}

impl<'ctx> Context<'ctx> {
    pub fn new(source: &'ctx str) -> Self {
        Self { source }
    }

    pub fn get_source(&self) -> &str {
        self.source
    }
}

// Run proc on every input and return the vec of the results or the first
// PrintableError.
pub fn map_result<InputT, OutputT, ErrT, F: FnMut(InputT) -> Result<OutputT, ErrT>>(items: Vec<InputT>, mut proc: F) -> Result<Vec<OutputT>, ErrT> {
    let mut passing = vec![];
    for item in items {
        passing.push(proc(item)?);
    }
    Ok(passing)
}
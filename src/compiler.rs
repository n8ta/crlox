use crate::scanner::{Scanner};

fn compile(src: String) {
    let mut scanner = Scanner::new(src);
    let line = -1;
    loop {
        let token = scanner.scan_token();
        if let Some(token) = scanner.scan_token() {

        } else {
            // EOF
        }
    }
}
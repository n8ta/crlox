use std::cmp::{min, max};
use std::rc::Rc;
use std::fmt::{Display, Formatter};
use colored::*;

#[derive(Clone, PartialOrd, PartialEq, Ord, Eq, Debug)]
pub struct Source {
    pub src: String,
}

impl Source {
    pub fn simple() -> Source {
        Source { src: String::from("") }
    }
    pub fn new(src: String) -> Source {
        Source { src }
    }

    pub fn start_point(&self, offset: usize) -> usize {
        let mut seen_line = false;
        let chars: Vec<char> = self.src.chars().collect::<Vec<char>>();
        for (i, chr) in chars.iter().rev().skip(chars.len()-offset).enumerate() {
            if *chr == '\n' && seen_line {
                return offset-i
            } else if *chr == '\n' {
                seen_line = true;
            }
        }
        0
    }

    pub fn end_point(&self, offset: usize) -> usize {
        let mut seen_line = false;
        let chars: Vec<char> = self.src.chars().collect::<Vec<char>>();

        for (i, chr) in chars.iter().skip(offset).enumerate() {
            if *chr == '\n' && seen_line {
                return offset+i;
            } else if *chr == '\n' {
                seen_line = true;
            }
        }
        self.src.chars().count()
    }

    pub fn prior_line(&self, offset: usize) -> &str {
        return &self.src[self.start_point(offset)..offset]
    }
    pub fn next_line(&self, offset: usize) -> &str {
        return &self.src[offset..self.end_point(offset)];
    }
}

#[derive(Clone, PartialOrd, PartialEq, Ord, Eq, Debug)]
pub struct SourceRef {
    // Represents a section of a larger string
    pub line: usize,
    offset: usize,
    len: usize, // starting at start_line[offset]
    src: Rc<Source>,
}

impl SourceRef {
    #[allow(dead_code)]
    pub fn simple() -> SourceRef {
        SourceRef { line: 0, offset: 0, len: 0, src: Rc::new(Source::simple()) }
    }
    pub fn new(offset: usize, len: usize, line: usize, src: Rc<Source>) -> SourceRef {
        SourceRef { offset, len, src, line }
    }
    pub fn merge(&self, other: &SourceRef) -> SourceRef {
        let start =  min(self.offset, other.offset);
        let end = max(self.offset + self.len, other.offset + other.len);
        SourceRef {
            offset: start,
            len: end - start,
            src: self.src.clone(),
            line: min(self.offset, other.offset),
        }
    }
    pub fn source(&self) -> String {
        self.src.src.chars().skip(self.offset).take(self.len).collect()
    }
}

impl Display for SourceRef {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let source = self.src.src.chars().skip(self.offset).take(self.len).collect::<String>().red().to_string();
        let before = self.src.prior_line(self.offset);
        let after = self.src.next_line(self.offset+self.len);

        let combined = format!("{}{}{}", before, source, after);
        let raw_output = combined.split("\n").collect::<Vec<&str>>();
        let mut with_line_nums = vec![];
        let mut idx = if before.contains('\n') {
            self.line - 1
        } else {
            self.line
        };
        for line in raw_output.into_iter() {
            let line_number = format!("\t[{}]\t", idx+1).white();
            with_line_nums.push(
                format!("{} {}", line_number, line)
            );
            idx += 1;
        }
        write!(f, "{}", with_line_nums.join("\n"))
    }
}

#[test]
fn merging() {
    let src = Rc::new(Source::simple());
    let a = SourceRef::new(0, 2, 0, src.clone());
    let b = SourceRef::new(6, 2, 0, src.clone());
    let c = SourceRef::new(1, 2, 0, src.clone());

    assert_eq!(a.merge(&b), SourceRef::new(0, 8, 0, src.clone()));
    assert_eq!(a.merge(&c), SourceRef::new(0, 3, 0, src.clone()));
    assert_ne!(a.merge(&c), SourceRef::new(0, 4, 0, src.clone()));

    assert_eq!(a.merge(&c), c.merge(&a));
    assert_eq!(a.merge(&b), b.merge(&a));
    assert_eq!(b.merge(&c), c.merge(&b));
}

#[test]
fn source() {
    let src = Rc::new(Source::new("this is my source".to_string()));
    let a = SourceRef::new(0, 4, 0, src.clone());
    let b = SourceRef::new(5, 2, 0, src.clone());
    let c = SourceRef::new(0, src.src.chars().count(), 0, src.clone());

    assert_eq!(a.source(), "this");
    assert_eq!(b.source(), "is");
    assert_eq!(c.source(), "this is my source");
}

#[test]
fn test_source_lines() {
    let src = Rc::new(Source::new(format!("line1\nline2\nline3\nline4")));
    assert_eq!(src.prior_line(0), "");
    assert_eq!(src.prior_line(1), "l");
    assert_eq!(src.prior_line(6), "line1\n");
    assert_eq!(src.prior_line(7), "line1\nl");
    assert_eq!(src.prior_line(12), "line2\n");
    assert_eq!(src.prior_line(16), "line2\nline");
    assert_eq!(src.next_line(0), "line1\nline2");
    assert_eq!(src.next_line(3), "e1\nline2");
    assert_eq!(src.next_line(5), "\nline2");
    assert_eq!(src.next_line(6), "line2\nline3");
}
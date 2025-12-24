use colored::Colorize;

/// Given some input and the begin position of a token, returns the (begin, end) indices of the
/// line that the token is on
fn get_token_context(input: &str, tok: usize) -> Option<(usize, usize, usize)> {
    let mut begin = 0;
    for (line_num, line) in input.lines().enumerate() {
        let end = begin + line.len();

        if (begin <= tok) || (tok <= end) {
            return Some((begin, end, line_num + 1));
        } else {
            begin += line.len() + "\n".len();
        }
    }

    None
}

/// Produces a formatted compiler error message
///
/// If the end of the error token is unknown (indicated by passing None to @param end), then the
/// error underline points to the beginning of the error. Otherwise it underlines the erronious
/// token
pub fn format_error(
    input: &str,
    begin: usize,
    end: Option<usize>,
    reason: &str,
    help: Option<&str>,
) -> String {
    let (line_begin, line_end, line_num) = get_token_context(input, begin).unwrap();
    let line = &input[line_begin..line_end];

    let underline = if let Some(end) = end {
        " ".repeat(begin - line_begin)
            + &"^".repeat(end - begin)
            + &format!(" ---- error occurs here (line {})", line_num)
    } else {
        " ".repeat(begin - line_begin) + &format!("^ ---- error occurs here (line {})", line_num)
    };

    let ident = " |".white().bold();

    let msg = format!(
        "{}{}\n {}\n {} {}\n {} {}",
        "Error".red().bold(),
        format!(": {}", reason).white().bold(),
        ident,
        ident,
        line,
        ident,
        underline.red().bold()
    );

    if let Some(help) = help {
        format!("{}\n\nHelp: {}", msg, help)
    } else {
        msg
    }
}

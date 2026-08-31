module LogLevels

let message (logLine: string): string = 
    let ind = logLine.IndexOf(":");
    logLine[ind + 1..].Trim();

let logLevel(logLine: string): string = 
    let indo = logLine.IndexOf("[")
    let indc = logLine.IndexOf("]")
    logLine[indo + 1..indc - 1].Trim().ToLower();

let reformat(logLine: string): string = 
    (message logLine) + " (" + (logLevel logLine) + ")"

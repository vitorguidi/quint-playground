module hello {

    var consoleOutput: str    
    var readByUser: str

    action init = all {
        consoleOutput' = "",
        readByUser' = "",
    }

    action write = all {
        consoleOutput == "",
        consoleOutput' = "Hello World!",
        readByUser' = readByUser,
    }

    action read = all {
        consoleOutput != "",
        consoleOutput' = consoleOutput,
        readByUser' = consoleOutput,
    }

    action step = any {
        read,
        write,
    }
    run writeReadTest = init.then(write).then(read)

}

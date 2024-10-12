/*
https://www.tutorialspoint.com/petersons-algorithm-in-process-synchronization
Process 0:
while (true) { -> not doing the loop otherwise too complex
   flag[0] = true;
   turn = 1;
   while (flag[1] && turn == 1) {} // busy wait
   // critical section
   flag[0] = false;
   // remainder section
}

Process 1:
while (true) {
start:
   flag[1] = true;
s1:
   turn = 0;
s2:
   while (flag[0] && turn == 0) {} // busy wait
   // critical section
cs:
   flag[1] = false;
done:
   // remainder section
}
*/

module peterson {

    type ProcessId = int
    type ProcessState = str

    val processes = 0.to(1)
    val possibleStates = Set("start", "s1", "s2", "s3", "done")

    var wantsToEnter: ProcessId -> bool
    var turn: int
    var iteration: int
    var processStates: ProcessId -> ProcessState


    action init = all {
        nondet whoStarts: int = oneOf(processes)
        val initialStates = Map(0->"start", 1->"start")
        val initialWantsToEnter = Map(0->false, 1->false)
        all {
            wantsToEnter' = initialWantsToEnter,
            iteration' = 0,
            processStates' = initialStates,
            turn' = whoStarts,
        }
    }

    action step_start(proc: int): bool = all {
        processStates.get(proc) == "start",
        turn' = turn,
        wantsToEnter' = wantsToEnter.set(proc, true),
        iteration' = iteration + 1,
        processStates' = processStates.set(proc, "s1"),
    }

    action step_s1(proc: int): bool = all {
        processStates.get(proc) == "s1",
        turn' = proc,
        iteration' = iteration + 1,
        wantsToEnter' = wantsToEnter,
        processStates' = processStates.set(proc, "s2")
    }

    action step_s2(proc: int): bool = all {
        processStates.get(proc) == "s2",
        turn' = proc,
        iteration' = iteration + 1,
        wantsToEnter' = wantsToEnter,
        if (turn==proc and wantsToEnter.get(proc)==true) {
            processStates' = processStates
        } else {
            processStates' = processStates.set(proc, "cs")
        }
    }

    action step_cs(proc: int): bool = all {
        processStates.get(proc) == "cs",
        wantsToEnter' = wantsToEnter.set(proc, false),
        turn' = turn,
        processStates' = processStates.set(proc, "done"),
        iteration' = iteration + 1,
    }

    action step: bool = all {
        nondet proc = oneOf(processes)
        any {
            step_start(proc),
            step_s1(proc),
            step_s2(proc),
            step_cs(proc),
        }
    }

    def mutual_exclusion(): bool = not(
        and(
            processStates.get(0) == "cs",
            processStates.get(1) == "cs"
        )
    )

    def completion(): bool = and(
        processStates.get(0) == "done",
        processStates.get(1) == "done"
    )

    temporal mutex: bool = always(mutual_exclusion)
    temporal finishes: bool = eventually(always(completion))

}

package main

import (
    "fmt"
    "math/rand"
    "strconv"
    "sync"
    "time"
)

const (
    philosopherNumber = 5
    lowerSleepTime    = 20
    upperSleepTime    = 100
    mealsCount        = 20
)

func getRandNum(lower, upper int) int {
    return rand.Intn(upper-lower+1) + lower
}

func printMessage(output *sync.Mutex, message string) {
    output.Lock()
    defer output.Unlock()
    fmt.Println(message)
}

func printThinkingMessage(id int, output *sync.Mutex) {
    printMessage(output, "Philosopher "+strconv.Itoa(id)+" is thinking")
}

func printEatingMessage(id int, output *sync.Mutex) {
    printMessage(output, "Philosopher "+strconv.Itoa(id)+" is eating")
}

func philosopherBehaviour(id int, leftFork, rightFork, output *sync.Mutex) {
    mealsEaten := 0
    for mealsEaten < mealsCount {
        printThinkingMessage(id, output)
        time.Sleep(time.Millisecond * time.Duration(getRandNum(lowerSleepTime, upperSleepTime)))

        leftFork.Lock()
        rightFork.Lock()

        printEatingMessage(id, output)
        time.Sleep(time.Millisecond * time.Duration(getRandNum(lowerSleepTime, upperSleepTime)))
        mealsEaten++

        rightFork.Unlock()
        leftFork.Unlock()
    }
	printMessage(output, "Philosopher "+strconv.Itoa(id)+" has finished eating his meals")
}

func main() {
    var forks [philosopherNumber]*sync.Mutex
    for i := range forks {
        forks[i] = &sync.Mutex{}
    }
    outputMutex := &sync.Mutex{}

    var wg sync.WaitGroup
    for i := 0; i < philosopherNumber; i++ {
        fmt.Printf("Philosopher %d created\n", i)
        wg.Add(1)
        go func(id int) {
            defer wg.Done()
            philosopherBehaviour(id, forks[id], forks[(id+1)%philosopherNumber], outputMutex)
        }(i)
    }

    wg.Wait()
}

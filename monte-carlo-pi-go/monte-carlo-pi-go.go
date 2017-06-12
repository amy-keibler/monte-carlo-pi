package main

import (
	"fmt"
	"math/rand"
	"os"
	"runtime"
	"strconv"
	"time"
)

func main() {
	points, err := strconv.Atoi(os.Args[1])
	if err == nil {
		fmt.Printf("Pi estimated to be: %f (%d data points used)", concEstimatePi(points), points)
	} else {
		println("That's not a number, dummy.")
	}
}

/**
Single threaded estimation. Here for comparison.
*/
func estimatePi(points int) float64 {
	withinCircle := 0
	gen := rand.New(rand.NewSource(time.Now().UnixNano()))
	for i := 0; i < points; i++ {
		x, y := generateCoordinate(gen)
		if x*x+y*y <= 1 {
			withinCircle++
		}
	}
	return 4 * float64(withinCircle) / float64(points)
}

/**
Multi threaded estimation.
*/
func concEstimatePi(points int) float64 {
	estimates := make(chan float64)
	defer close(estimates)
	cores := runtime.NumCPU()
	for c := 0; c < cores; c++ {
		gen := rand.New(rand.NewSource(time.Now().UnixNano()))
		go channledEstimatePi(points/cores, estimates, gen)
	}
	sumEstimate := float64(0)
	for c := 0; c < cores; c++ {
		sumEstimate += <-estimates
	}
	return sumEstimate / 4
}

func channledEstimatePi(points int, estimate chan float64, gen *rand.Rand) {
	withinCircle := 0
	for i := 0; i < points; i++ {
		x, y := generateCoordinate(gen)
		if x*x+y*y <= 1 {
			withinCircle++
		}
	}
	estimate <- 4 * float64(withinCircle) / float64(points)
}

func generateCoordinate(gen *rand.Rand) (float64, float64) {
	return (gen.Float64() * 2) - 1, (gen.Float64() * 2) - 1
}

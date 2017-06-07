package main

import (
	"os"
	"strconv"
	"math/rand"
	"fmt"
)

func main() {
	points, err := strconv.Atoi(os.Args[1])
	if err == nil {
		fmt.Printf("Pi estimated to be: %f (%d data points used)", estimatePi(points), points)
	} else {
		println("That's not a number, dummy.")
	}
}

func estimatePi(points int) (float64) {
	withinCircle := 0
	for i := 0; i < points; i++ {
		x, y := generateCoordinate()
		if x*x + y*y <= 1 {
			withinCircle++
		}
	}
	return 4 * float64(withinCircle)/float64(points)
}

func generateCoordinate() (float64, float64) {
	return (rand.Float64() * 2) - 1, (rand.Float64() * 2) - 1
}

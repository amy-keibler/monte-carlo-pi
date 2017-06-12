package main

import (
	"testing"
)

func BenchmarkSingleThreaded(b *testing.B) {
	for i := 0; i < b.N; i++ {
		estimatePi(100000)
	}
}

func BenchmarkMultiThreaded(b *testing.B) {
	for i := 0; i < b.N; i++ {
		concEstimatePi(100000)
	}
}

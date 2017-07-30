package main

import (
	"os"
	"testing"
)

func Benchmark_processFile(b *testing.B) {
	file, err := os.Open("../ngrams.tsv")
	defer file.Close()
	if err != nil {
		b.Fatal(err)
	}
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		k, v := processFile(file)
		if k != 2006 || v != 22569013 {
			b.Fatalf(`bad result %v | %v`, k, v)
		}
	}
}

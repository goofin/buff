package buff

import (
	"math/bits"
	"testing"
)

var table = [256]uint8{}

func init() {
	for i := range table {
		table[i] = uint8(bits.LeadingZeros8(^uint8(i)))
	}
}

func leadingOnes(x uint8) uint8 {
	return table[x]
}

var sum uint8

func BenchmarkLeadingOnes(b *testing.B) {
	for i := 0; i < b.N; i++ {
		sum += leadingOnes(uint8(i))
	}
}

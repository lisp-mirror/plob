	.seg	"text"
	.global	mick_SS_Xmemlock
mick_SS_Xmemlock:
	ldstub	[%o0 + %g0],%o1
	subcc	%o1,128,%o1
	bneg	1f
	nop
	or	%g0,%g0,%o0
	ba	2f
	nop
1:
	ld	[%o0 + %g0],%o0
2:
	jmpl	%o7+8,%g0
	nop

# Local variables:
# buffer-file-coding-system: iso-latin-1-unix
# End:

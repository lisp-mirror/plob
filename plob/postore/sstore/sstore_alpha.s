	.text	
	.align	4
	.globl	SS_Xmemlock
 #    1	int SS_Xmemlock( real_addr )
 #    2	int *real_addr ;
 #    3	{
	.ent	SS_Xmemlock
SS_Xmemlock:
	.frame	$sp,0,$26
	mov	255,$2
	sll	$2,24,$2

	ldl_l	$1,($16)
	bge	$1,1f
	stl_c	$1,($16)
	bis	$31, $31, $0
	ret	$31, ($26), 1
1:
	bis	$1,$2,$1
	mov	$1,$2
	stl_c	$2,($16)
	cmoveq	$2,$31,$0
	cmovne	$2,$1,$0
	ret	$31, ($26), 1
	.end	SS_Xmemlock

# Local variables:
# buffer-file-coding-system: iso-latin-1-unix
# End:

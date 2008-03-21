
	.global main 
	.align 4
.str9:
	.asciz "in g: %d\n"
.str10:
	.asciz "in f: %d\n"

	.align 4
main:

	save %sp,-200,%sp
	mov 10,%o0
	st %o0,[%fp-20]
	ba endfunc3
	nop
func1:
	save %sp,-200,%sp
	st %i0,[%fp-20]
	set .str9,%o0
	mov %i0,%o1
	call printf
	nop
	mov 0,%o0
	cmp %i0,%o0
	mov 1,%o0
	bg bool4
	nop
	mov %g0,%o0
bool4:
	cmp %o0,1
	bne else5
	nop
	mov 1,%o0
	sub %i0,%o0,%o0
	call func2
	nop
	ba endif5
	nop
else5:
endif5:
	mov %o0,%i0
	ret
	restore
endfunc3:
	ba endfunc6
	nop
func2:
	save %sp,-200,%sp
	st %i0,[%fp-20]
	set .str10,%o0
	mov %i0,%o1
	call printf
	nop
	mov 0,%o0
	cmp %i0,%o0
	mov 1,%o0
	bg bool7
	nop
	mov %g0,%o0
bool7:
	cmp %o0,1
	bne else8
	nop
	mov 1,%o0
	sub %i0,%o0,%o0
	call func1
	nop
	ba endif8
	nop
else8:
endif8:
	mov %o0,%i0
	ret
	restore
endfunc6:
	ld [%fp-20],%o0
	call func1
	nop

	mov 1, %g1
	ta 0

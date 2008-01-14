
	.global main 
	.align 4
.str4:
	.asciz "a in this scope is: %d\n"
.str5:
	.asciz "7 if static, 9 if dynamic: %d\n"

	.align 4
main:

	save %sp,-200,%sp
	mov 2,%o0
	st %o0,[%fp-20]
	ba endfunc2
	nop
func1:
	save %sp,-200,%sp
	st %i0,[%fp-20]
	ld [%fp+180],%o0
	add %i0,%o0,%o0
	mov %o0,%i0
	ret
	restore
endfunc2:
	ld [%fp-20],%o0
	call func1
	nop
	st %o0,[%fp-24]
	set .str4,%o0
	ld [%fp-24],%o1
	call printf
	nop
	set .str5,%o0
	mov 5,%o1
	mov %o1,%l0
	mov %l0,%o0
	call func1
	nop
	mov %o0,%l1
	mov %l1,%o1
	call printf
	nop

	mov 1, %g1
	ta 0

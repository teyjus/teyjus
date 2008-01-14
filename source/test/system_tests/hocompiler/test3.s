
	.global main 
	.align 4
.str24:
	.asciz "I love compiling!\n"
.str25:
	.asciz "n = %d\n"
.str26:
	.asciz "z is finally %d\n"
.str27:
	.asciz "z is big\n"
.str28:
	.asciz "z is small\n"
.str29:
	.asciz "z is tiny\n"
.str30:
	.asciz "the factorial of 6 is %d\n"
.str31:
	.asciz "with tail recursion it's also %d\n"
.str32:
	.asciz "without recursion it's still %d\n"

	.align 4
main:

	save %sp,-200,%sp
	mov 5,%o0
	st %o0,[%fp-20]
	set .str24,%o0
	st %o0,[%fp-24]
	ba endfunc6
	nop
func2:
	save %sp,-200,%sp
	st %i0,[%fp-20]
	st %i1,[%fp-24]
	add %i0,%i1,%o0
	mov 1,%o1
	sub %o0,%o1,%o0
	mov %o0,%i0
	ret
	restore
endfunc6:
	ba endfunc7
	nop
func3:
	save %sp,-200,%sp
	st %i0,[%fp-20]
	mov 2,%o0
	cmp %i0,%o0
	mov 1,%o0
	bl bool8
	nop
	mov %g0,%o0
bool8:
	cmp %o0,1
	bne else9
	nop
	mov %i0,%o0
	ba endif9
	nop
else9:
	mov 1,%o0
	sub %i0,%o0,%o0
	call func3
	nop
	mov %o0,%l0
	mov %i0,%o0
	mov %l0,%o1
	call .umul
	nop
endif9:
	mov %o0,%i0
	ret
	restore
endfunc7:
	ba endfunc10
	nop
func4:
	save %sp,-200,%sp
	st %i0,[%fp-20]
	st %i1,[%fp-24]
	mov 2,%o0
	cmp %i0,%o0
	mov 1,%o0
	bl bool11
	nop
	mov %g0,%o0
bool11:
	cmp %o0,1
	bne else12
	nop
	mov %i1,%o0
	ba endif12
	nop
else12:
	mov 1,%o0
	sub %i0,%o0,%o0
	mov %i1,%o0
	mov %i0,%o1
	call .umul
	nop
	mov %o0,%l0
	mov %l0,%o1
	call func4
	nop
endif12:
	mov %o0,%i0
	ret
	restore
endfunc10:
	ba endfunc13
	nop
func5:
	save %sp,-200,%sp
	st %i0,[%fp-20]
	mov 1,%o0
	st %o0,[%fp-24]
	st %i0,[%fp-28]
swhile16:
	ld [%fp-28],%o0
	mov 1,%o1
	cmp %o0,%o1
	mov 1,%o0
	bg bool17
	nop
	mov %g0,%o0
bool17:
	cmp %o0,%g0
	be ewhile16
	nop
	ld [%fp-24],%o0
	ld [%fp-28],%o1
	call .umul
	nop
	st %o0,[%fp-24]
	ld [%fp-28],%o0
	mov 1,%o1
	sub %o0,%o1,%o0
	st %o0,[%fp-28]
	ba swhile16
	nop
ewhile16:
	ld [%fp-24],%o0
	mov %o0,%i0
	ret
	restore
endfunc13:
	nop
swhile18:
	ld [%fp-20],%o0
	mov 1,%o1
	cmp %o0,%o1
	mov 1,%o0
	bg bool19
	nop
	mov %g0,%o0
bool19:
	cmp %o0,%g0
	be ewhile18
	nop
	ld [%fp-20],%o0
	mov 1,%o1
	sub %o0,%o1,%o0
	st %o0,[%fp-20]
	set .str25,%o0
	ld [%fp-20],%o1
	call printf
	nop
	ba swhile18
	nop
ewhile18:
	mov 4,%o0
	ld [%fp-20],%o1
	mov 2,%o2
	add %o1,%o2,%o1
	call func2
	nop
	st %o0,[%fp-20]
	set .str26,%o0
	ld [%fp-20],%o1
	call printf
	nop
	ld [%fp-20],%o0
	mov 10,%o1
	cmp %o0,%o1
	mov 1,%o0
	bg bool20
	nop
	mov %g0,%o0
bool20:
	cmp %o0,1
	bne else21
	nop
	set .str27,%o0
	call printf
	nop
	ba endif21
	nop
else21:
	ld [%fp-20],%o0
	mov 2,%o1
	cmp %o0,%o1
	mov 1,%o0
	bg bool22
	nop
	mov %g0,%o0
bool22:
	cmp %o0,1
	bne else23
	nop
	set .str28,%o0
	call printf
	nop
	ba endif23
	nop
else23:
	set .str29,%o0
	call printf
	nop
endif23:
endif21:
	mov 6,%o0
	call func3
	nop
	st %o0,[%fp-20]
	set .str30,%o0
	ld [%fp-20],%o1
	call printf
	nop
	set .str31,%o0
	mov 6,%o1
	mov %o1,%l0
	mov 1,%o0
	mov %o0,%l1
	mov %l0,%o0
	mov %l1,%o1
	call func4
	nop
	mov %o0,%l2
	mov %l2,%o1
	call printf
	nop
	set .str32,%o0
	mov 6,%o1
	mov %o1,%l0
	mov %l0,%o0
	call func5
	nop
	mov %o0,%l1
	mov %l1,%o1
	call printf
	nop
	ld [%fp-24],%o0
	call printf
	nop

	mov 1, %g1
	ta 0

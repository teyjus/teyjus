
	.global main 
	.align 4
.str14:
	.asciz "%d\n"
.str15:
	.asciz "class c2, x=%d\n"
.str16:
	.asciz "My balance is %d\n"
.str17:
	.asciz "Your balance is %d\n"

	.align 4
main:

	save %sp,-200,%sp
	ba endfunc7
	nop
class6make:
	save %sp,-200,%sp
	st %i0,[%fp-20]
	st %i1,[%fp-24]
	st %i0,[%i5-4]
	st %i1,[%i5]
	set .str14,%o0
	ld [%i5],%o1
	call printf
	nop
	mov %o0,%i0
	ret
	restore
endfunc7:
	ba endfunc8
	nop
class6withdraw:
	save %sp,-200,%sp
	st %i0,[%fp-20]
	ld [%i5-4],%o0
	sub %o0,%i0,%o0
	st %o0,[%i5-4]
	mov %o0,%i0
	ret
	restore
endfunc8:
	ba endfunc9
	nop
class6deposit:
	save %sp,-200,%sp
	st %i0,[%fp-20]
	ld [%i5-4],%o0
	add %o0,%i0,%o0
	st %o0,[%i5-4]
	mov %o0,%i0
	ret
	restore
endfunc9:
	ba endfunc10
	nop
class6inquiry:
	save %sp,-200,%sp
	ld [%i5-4],%o0
	mov %o0,%i0
	ret
	restore
endfunc10:
	ba endfunc12
	nop
class11make:
	save %sp,-200,%sp
	st %i0,[%fp-20]
	st %i0,[%i5]
	set .str15,%o0
	ld [%i5],%o1
	call printf
	nop
	mov %o0,%i0
	ret
	restore
endfunc12:
	mov 0,%o0
	mov %o0,%g7
	mov 8,%o0
	call malloc
	nop
	st %o0,[%fp-20]
	mov %g7,%o0
	mov 0,%o1
	mov %o0,%g7
	mov 8,%o0
	call malloc
	nop
	st %o0,[%fp-24]
	mov %g7,%o0
	mov 0,%o2
	mov %o0,%g7
	mov 4,%o0
	call malloc
	nop
	st %o0,[%fp-28]
	mov %g7,%o0
	ba endfunc13
	nop
func5:
	save %sp,-200,%sp
	mov 4,%o0
	mov %o0,%i0
	ret
	restore
endfunc13:
	mov 6,%o3
	mov %o3,%l0
	mov %l0,%o0
	ld [%fp-28],%o5
	call class11make
	nop
	mov 100,%o3
	mov %o3,%l0
	mov 3421,%o0
	mov %o0,%l1
	mov %l0,%o0
	mov %l1,%o1
	ld [%fp-20],%o5
	call class6make
	nop
	mov 200,%o3
	mov %o3,%l0
	mov 1234,%o0
	mov %o0,%l1
	mov %l0,%o0
	mov %l1,%o1
	ld [%fp-24],%o5
	call class6make
	nop
	mov 40,%o3
	mov %o3,%l0
	mov %l0,%o0
	ld [%fp-20],%o5
	call class6withdraw
	nop
	mov 50,%o3
	mov %o3,%l0
	mov %l0,%o0
	ld [%fp-24],%o5
	call class6deposit
	nop
	set .str16,%o3
	mov %o3,%l0
	ld [%fp-20],%o5
	call class6inquiry
	nop
	mov %o0,%l1
	mov %l0,%o0
	mov %l1,%o1
	call printf
	nop
	set .str17,%o3
	mov %o3,%l0
	ld [%fp-24],%o5
	call class6inquiry
	nop
	mov %o0,%l1
	mov %l0,%o0
	mov %l1,%o1
	call printf
	nop

	mov 1, %g1
	ta 0


	.global main 
	.align 4
.str8:
	.asciz "hello\n"
.str9:
	.asciz "here1\n"
.str10:
	.asciz "a is now %d, and x is now %d\n"
.str11:
	.asciz "%d "
.str12:
	.asciz "\n"

	.align 4
main:

	save %sp,-200,%sp
	mov 6,%o0
	st %o0,[%fp-20]
	mov 0,%o0
	st %o0,[%fp-24]
	set .str8,%o0
	st %o0,[%fp-28]
	mov 20,%o0
	st %o0,[%fp-32]
	st %o0,[%fp-36]
	st %o0,[%fp-40]
	st %o0,[%fp-44]
	st %o0,[%fp-48]
	ba endfunc5
	nop
func4:
	save %sp,-200,%sp
	st %i0,[%fp-20]
	ld [%fp+180],%o0
	add %o0,%i0,%o0
	st %o0,[%fp+180]
	mov %o0,%i0
	ret
	restore
endfunc5:
	ld [%fp-28],%o1
	mov %o1,%l0
	mov %l0,%o0
	call printf
	nop
	mov 4,%o1
	mov %o1,%l0
	mov %l0,%o0
	call func4
	nop
	st %o0,[%fp-24]
	mov 4,%o1
	mov %o1,%l0
	mov %l0,%o0
	call func4
	nop
	st %o0,[%fp-24]
	set .str9,%o1
	mov %o1,%l0
	mov %l0,%o0
	call printf
	nop
	set .str10,%o1
	mov %o1,%l0
	ld [%fp-20],%o0
	mov %o0,%l1
	ld [%fp-24],%o0
	mov %o0,%l2
	mov %l0,%o0
	mov %l1,%o1
	mov %l2,%o2
	call printf
	nop
	mov 4,%o1
	st %o1,[%fp-20]
	mov 1,%o1
	mov 0,%o2
	mov -32,%g7
	mov %o2,%o3
	sll %o3,2,%o3
	sub %g7,%o3,%o3
	st %o1,[%fp+%o3]
	mov 2,%o1
	mov 1,%o2
	mov -32,%g7
	mov %o2,%o3
	sll %o3,2,%o3
	sub %g7,%o3,%o3
	st %o1,[%fp+%o3]
	mov 3,%o1
	mov 2,%o2
	mov -32,%g7
	mov %o2,%o3
	sll %o3,2,%o3
	sub %g7,%o3,%o3
	st %o1,[%fp+%o3]
	mov 4,%o1
	mov 3,%o2
	mov -32,%g7
	mov %o2,%o3
	sll %o3,2,%o3
	sub %g7,%o3,%o3
	st %o1,[%fp+%o3]
	mov 5,%o1
	mov 4,%o2
	mov -32,%g7
	mov %o2,%o3
	sll %o3,2,%o3
	sub %g7,%o3,%o3
	st %o1,[%fp+%o3]
swhile6:
	ld [%fp-20],%o1
	mov 0,%o2
	cmp %o1,%o2
	mov 1,%o1
	bge bool7
	nop
	mov %g0,%o1
bool7:
	cmp %o1,%g0
	be ewhile6
	nop
	set .str11,%o1
	mov %o1,%l0
	ld [%fp-20],%o0
	sll %o0,2,%o0
	mov -32,%g7
	sub %g7,%o0,%o0
	ld [%fp+%o0],%o0
	mov %o0,%l1
	mov %l0,%o0
	mov %l1,%o1
	call printf
	nop
	ld [%fp-20],%o1
	mov 1,%o2
	sub %o1,%o2,%o1
	st %o1,[%fp-20]
	ba swhile6
	nop
ewhile6:
	set .str12,%o1
	mov %o1,%l0
	mov %l0,%o0
	call printf
	nop

	mov 1, %g1
	ta 0

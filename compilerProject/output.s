	.text
	.file	"main"
	.globl	main                            # -- Begin function main
	.p2align	4, 0x90
	.type	main,@function
main:                                   # @main
	.cfi_startproc
# %bb.0:                                # %entry
	pushq	%rax
	.cfi_def_cfa_offset 16
	movl	$.L__unnamed_1, %edi
	movl	$.L__unnamed_2, %esi
	xorl	%eax, %eax
	callq	screenit@PLT
	movl	$42, 4(%rsp)
	movl	$.L__unnamed_3, %edi
	movl	$42, %esi
	xorl	%eax, %eax
	callq	screenit@PLT
	movl	$15, (%rsp)
	movl	$.L__unnamed_4, %edi
	movl	$15, %esi
	xorl	%eax, %eax
	callq	screenit@PLT
	movl	$.L__unnamed_5, %edi
	movl	$.L__unnamed_6, %esi
	xorl	%eax, %eax
	callq	screenit@PLT
	movl	4(%rsp), %esi
	addl	(%rsp), %esi
	movl	$.L__unnamed_7, %edi
	xorl	%eax, %eax
	callq	screenit@PLT
	xorl	%eax, %eax
	popq	%rcx
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end0:
	.size	main, .Lfunc_end0-main
	.cfi_endproc
                                        # -- End function
	.type	.L__unnamed_2,@object           # @0
	.section	.rodata.str1.1,"aMS",@progbits,1
.L__unnamed_2:
	.asciz	"Hello from Gran!"
	.size	.L__unnamed_2, 17

	.type	.L__unnamed_1,@object           # @1
.L__unnamed_1:
	.asciz	"%s\n"
	.size	.L__unnamed_1, 4

	.type	.L__unnamed_3,@object           # @2
.L__unnamed_3:
	.asciz	"%d\n"
	.size	.L__unnamed_3, 4

	.type	.L__unnamed_4,@object           # @3
.L__unnamed_4:
	.asciz	"%d\n"
	.size	.L__unnamed_4, 4

	.type	.L__unnamed_6,@object           # @4
.L__unnamed_6:
	.asciz	"The sum is:"
	.size	.L__unnamed_6, 12

	.type	.L__unnamed_5,@object           # @5
.L__unnamed_5:
	.asciz	"%s\n"
	.size	.L__unnamed_5, 4

	.type	.L__unnamed_7,@object           # @6
.L__unnamed_7:
	.asciz	"%d\n"
	.size	.L__unnamed_7, 4

	.section	".note.GNU-stack","",@progbits

# -- Machine type PW
# mark_description "Intel(R) Fortran Compiler for 32-bit applications, Version 6.0   Build 020722Z";
# mark_description "-Xlinker -rpath -Xlinker /opt/intel/compiler60/ia32/lib -S -xW";
	.ident "Intel(R) Fortran Compiler for 32-bit applications, Version 6.0   Build 020722Z"
	.ident "-Xlinker -rpath -Xlinker /opt/intel/compiler60/ia32/lib -S -xW"
	.file "transp_4.f"
	.text
	.data
	.align 4
	.bss
	.align 4
	.ident "-?comment:Intel(R) Fortran Compiler for 32-bit applications, Version 6.0   Build 020722Z  : transp_4.f : -Xlinker -rpath -Xlinker /opt/intel/compiler60/ia32/lib -S -xW"
	.data
	.text
# -- Begin  transp_
# mark_begin;
       .align    4,0x90
# parameter 1: 72 + %esp
# parameter 2: 76 + %esp
# parameter 3: 80 + %esp
# parameter 4: 84 + %esp
	.globl   transp_
transp_:
.B1.1:                          # Preds .B1.0
        pushl     %edi                                          #5.0
        pushl     %esi                                          #5.0
        pushl     %ebp                                          #5.0
        pushl     %ebx                                          #5.0
        subl      $52, %esp                                     #5.0
        movl      72(%esp), %edx                                #5.0
        xorl      %ebx, %ebx                                    #11.0
        movl      76(%esp), %eax                                #5.0
        movl      (%eax), %eax                                  #11.0
        lea       -8(%eax), %ebp                                #11.0
        testl     %ebp, %ebp                                    #11.0
        jl        .B1.11        # Prob 2%                       #11.0
                                # LOE eax edx ebx ebp
.B1.2:                          # Preds .B1.1
        movl      (%edx), %esi                                  #12.0
        movl      %esi, 12(%esp)                                #13.0
        movl      %esi, %edx                                    #13.0
        imull     %ebx, %edx                                    #13.0
        lea       -8(%esi), %ecx                                #13.0
        lea       (%esi,%esi), %edi                             #13.0
        movl      %eax, 48(%esp)                                #13.0
        addl      %edi, %edi                                    #13.0
        addl      %edi, %edi                                    #13.0
        movl      %edi, 28(%esp)                                #13.0
                                # LOE edx ecx ebx ebp
.B1.3:                          # Preds .B1.2 .B1.9
        xorl      %edi, %edi                                    #13.0
        testl     %ecx, %ecx                                    #13.0
        jl        .B1.9         # Prob 2%                       #13.0
                                # LOE edx ecx ebx ebp edi
.B1.4:                          # Preds .B1.3
        movl      48(%esp), %eax                                #
        movl      %ebx, 40(%esp)                                #
        movl      %eax, %esi                                    #
        imull     %edi, %esi                                    #
        movl      %ebp, 44(%esp)                                #
        lea       (%eax,%eax), %eax                             #
        addl      %ebx, %esi                                    #
        addl      %eax, %eax                                    #
        addl      %eax, %eax                                    #
        movl      %eax, 20(%esp)                                #
                                # LOE edx ecx esi edi
.B1.5:                          # Preds .B1.4 .B1.7
        movl      %esi, 16(%esp)                                #16.0
        movl      %esi, %eax                                    #16.0
        movl      %edi, 24(%esp)                                #16.0
        movl      %edx, 32(%esp)                                #16.0
        lea       (%edx,%edi), %ebx                             #15.0
        lea       7(%esi), %ebp                                 #16.0
        movl      80(%esp), %esi                                #16.0
        movl      %ebp, 8(%esp)                                 #16.0
        movl      48(%esp), %ebp                                #16.0
        movl      %ebx, 4(%esp)                                 #16.0
        movl      %ecx, 36(%esp)                                #16.0
        .align    4,0x90
                                # LOE eax ebp esi
.B1.6:                          # Preds .B1.6 .B1.5
        movl      4(%esp), %edx                                 #
        lea       (%edx,%edx), %ecx                             #
        movl      84(%esp), %edx                                #18.0
        movapd     (%esi,%ecx,8), %xmm0                          #18.0
        movapd     16(%esi,%ecx,8), %xmm1                        #20.0
        movapd     32(%esi,%ecx,8), %xmm2                        #22.0
        movapd     48(%esi,%ecx,8), %xmm3                        #24.0
        movapd     64(%esi,%ecx,8), %xmm4                        #26.0
        movapd     80(%esi,%ecx,8), %xmm5                        #28.0
        movapd     96(%esi,%ecx,8), %xmm6                        #30.0
        movapd     112(%esi,%ecx,8), %xmm7                       #32.0
		movl       %eax, %ebx
        lea       (%ebx,%ebx), %edi                             #
        movapd     %xmm0, (%edx,%edi,8)                          #18.0
        addl      %ebp, %ebx
        lea       (%ebx,%ebx), %edi                             #
        movapd     %xmm1, (%edx,%edi,8)                          #20.0
        addl      %ebp, %ebx                                    #21.0
        lea       (%ebx,%ebx), %edi                             #
        movapd     %xmm2, (%edx,%edi,8)                          #22.0
        addl      %ebp, %ebx                                    #23.0
        lea       (%ebx,%ebx), %edi                             #
        movapd     %xmm3, (%edx,%edi,8)                          #24.0
        addl      %ebp, %ebx                                    #25.0
        lea       (%ebx,%ebx), %edi                             #
        movapd     %xmm4, (%edx,%edi,8)                          #26.0
        addl      %ebp, %ebx                                    #27.0
        lea       (%ebx,%ebx), %edi                             #
        movapd     %xmm5, (%edx,%edi,8)                          #28.0
        addl      %ebp, %ebx                                    #29.0
        lea       (%ebx,%ebx), %edi                             #
        movapd     %xmm6, (%edx,%edi,8)                          #30.0
        addl      %ebp, %ebx                                    #31.0
        lea       (%ebx,%ebx), %edi                             #
        movapd     %xmm7, (%edx,%edi,8)                          #32.0
        movl      84(%esp), %edx                                #32.0
        movl      8(%esp), %edi                                 #50.0
        movl      4(%esp), %edx                                 #49.0
        addl      12(%esp), %edx                                #49.0
        movl      %edx, 4(%esp)                                 #49.0
        addl      $1, %eax                                      #50.0
        cmpl      %eax, %edi                                    #50.0
        jge       .B1.6         # Prob 85%                      #50.0
                                # LOE eax ebp esi
.B1.7:                          # Preds .B1.6
        movl      16(%esp), %esi                                #
        addl      20(%esp), %esi                                #51.0
        movl      24(%esp), %edi                                #
        movl      32(%esp), %edx                                #
        movl      36(%esp), %ecx                                #
        addl      $8, %edi                                      #51.0
        cmpl      %ecx, %edi                                    #51.0
        jle       .B1.5         # Prob 90%                      #51.0
                                # LOE edx ecx esi edi dl dh cl ch
.B1.8:                          # Preds .B1.7
        movl      40(%esp), %ebx                                #
        movl      44(%esp), %ebp                                #
                                # LOE edx ecx ebx ebp
.B1.9:                          # Preds .B1.3 .B1.8
        addl      28(%esp), %edx                                #52.0
        addl      $8, %ebx                                      #52.0
        cmpl      %ebp, %ebx                                    #52.0
        jle       .B1.3         # Prob 90%                      #52.0
                                # LOE edx ecx ebx ebp
.B1.11:                         # Preds .B1.9 .B1.1
        addl      $52, %esp                                     #53.0
        popl      %ebx                                          #53.0
        popl      %ebp                                          #53.0
        popl      %esi                                          #53.0
        popl      %edi                                          #53.0
        ret                                                     #53.0
        .align    4,0x90
                                # LOE
# mark_end;
	.type	transp_,@function
	.size	transp_,.-transp_
	.data
# -- End  transp_
	.data
EPC.RZERO.0:
	.long 0x00000000,0x00000000,0x00000000,0x00000000	# xf128
	.type	EPC.RZERO.0,@object
	.size	EPC.RZERO.0,16
	.data
# End

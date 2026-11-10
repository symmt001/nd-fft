# -- Machine type PW
# mark_description "Intel(R) Fortran Compiler for 32-bit applications, Version 6.0   Build 020722Z";
# mark_description "-Xlinker -rpath -Xlinker /opt/intel/compiler60/ia32/lib -S -xW";
	.ident "Intel(R) Fortran Compiler for 32-bit applications, Version 6.0   Build 020722Z"
	.ident "-Xlinker -rpath -Xlinker /opt/intel/compiler60/ia32/lib -S -xW"
	.file "transp_3.f"
	.text
	.data
	.align 4
	.bss
	.align 4
	.ident "-?comment:Intel(R) Fortran Compiler for 32-bit applications, Version 6.0   Build 020722Z  : transp_3.f : -Xlinker -rpath -Xlinker /opt/intel/compiler60/ia32/lib -S -xW"
	.data
	.text
# -- Begin  transp_
# mark_begin;
       .align    4,0x90
# parameter 1: 52 + %esp
# parameter 2: 56 + %esp
# parameter 3: 60 + %esp
# parameter 4: 64 + %esp
	.globl   transp_
transp_:
.B1.1:                          # Preds .B1.0
        pushl     %edi                                          #5.0
        pushl     %esi                                          #5.0
        pushl     %ebp                                          #5.0
        pushl     %ebx                                          #5.0
        subl      $32, %esp                                     #5.0
        movl      52(%esp), %edx                                #5.0
        xorl      %eax, %eax                                    #11.0
        xorl      %ebp, %ebp                                    #12.0
        movl      56(%esp), %ecx                                #5.0
        movl      (%ecx), %ecx                                  #12.0
        lea       -32(%ecx), %edi                               #12.0
        testl     %edi, %edi                                    #12.0
        jl        .B1.13        # Prob 2%                       #12.0
                                # LOE eax edx ecx ebp edi
.B1.2:                          # Preds .B1.1
        movl      (%edx), %ebx                                  #14.0
        movl      %ebx, 28(%esp)                                #29.0
        movl      %ecx, 4(%esp)                                 #29.0
        lea       -32(%ebx), %edx                               #14.0
        lea       (%ebx,%ebx), %esi                             #29.0
        addl      %esi, %esi                                    #29.0
        addl      %esi, %esi                                    #29.0
        addl      %esi, %esi                                    #29.0
        addl      %esi, %esi                                    #29.0
                                # LOE eax edx ebp esi edi
.B1.3:                          # Preds .B1.2 .B1.11
        movl      %ebp, %ebx                                    #13.0
        xorl      %ecx, %ecx                                    #14.0
        testl     %edx, %edx                                    #14.0
        jl        .B1.11        # Prob 2%                       #14.0
                                # LOE eax edx ecx ebx ebp esi edi
.B1.4:                          # Preds .B1.3
        movl      %ebp, 20(%esp)                                #
        movl      4(%esp), %ebp                                 #
        movl      %esi, 16(%esp)                                #
        movl      %edi, 8(%esp)                                 #
                                # LOE eax edx ecx ebx ebp
.B1.5:                          # Preds .B1.4 .B1.9
        movl      60(%esp), %ebp                                #16.0
        movl      %ebx, 24(%esp)                                #16.0
        movl      28(%esp), %ebx                                #16.0
        lea       (%eax,%ecx), %edi                             #15.0
        xorl      %esi, %esi                                    #16.0
        movl      %edx, 12(%esp)                                #16.0
        .align    4,0x90
                                # LOE eax ecx ebx ebp esi edi
.B1.6:                          # Preds .B1.6 .B1.5
        lea       (%edi,%edi), %edx                             #18.0
        movapd     (%ebp,%edx,8), %xmm0                          #18.0
        movapd     16(%ebp,%edx,8), %xmm1                        #18.0
        movapd     32(%ebp,%edx,8), %xmm2                        #18.0
        movapd     48(%ebp,%edx,8), %xmm3                        #18.0
        movapd     64(%ebp,%edx,8), %xmm4                        #18.0
        movapd     80(%ebp,%edx,8), %xmm5                        #18.0
        movapd     96(%ebp,%edx,8), %xmm6                        #18.0
        movapd     112(%ebp,%edx,8), %xmm7                       #18.0
        movapd     %xmm0, ..1.transp_LOCAL.buf.6(%esi)           #18.0
        movapd     %xmm1, ..1.transp_LOCAL.buf.6+16(%esi)        #18.0
        movapd     %xmm2, ..1.transp_LOCAL.buf.6+32(%esi)        #18.0
        movapd     %xmm3, ..1.transp_LOCAL.buf.6+48(%esi)        #18.0
        movapd     %xmm4, ..1.transp_LOCAL.buf.6+64(%esi)        #18.0
        movapd     %xmm5, ..1.transp_LOCAL.buf.6+80(%esi)        #18.0
        movapd     %xmm6, ..1.transp_LOCAL.buf.6+96(%esi)        #18.0
        movapd     %xmm7, ..1.transp_LOCAL.buf.6+112(%esi)       #18.0
        movapd     128(%ebp,%edx,8), %xmm0                       #18.0
        movapd     144(%ebp,%edx,8), %xmm1                       #18.0
        movapd     160(%ebp,%edx,8), %xmm2                       #18.0
        movapd     176(%ebp,%edx,8), %xmm3                       #18.0
        movapd     192(%ebp,%edx,8), %xmm4                       #18.0
        movapd     208(%ebp,%edx,8), %xmm5                       #18.0
        movapd     224(%ebp,%edx,8), %xmm6                       #18.0
        movapd     240(%ebp,%edx,8), %xmm7                       #18.0
		movapd     %xmm0, ..1.transp_LOCAL.buf.6+128(%esi)       #18.0
        movapd     %xmm1, ..1.transp_LOCAL.buf.6+144(%esi)       #18.0
        movapd     %xmm2, ..1.transp_LOCAL.buf.6+160(%esi)       #18.0
        movapd     %xmm3, ..1.transp_LOCAL.buf.6+176(%esi)       #18.0
        movapd     %xmm4, ..1.transp_LOCAL.buf.6+192(%esi)       #18.0
        movapd     %xmm5, ..1.transp_LOCAL.buf.6+208(%esi)       #18.0
        movapd     %xmm6, ..1.transp_LOCAL.buf.6+224(%esi)       #18.0
        movapd     %xmm7, ..1.transp_LOCAL.buf.6+240(%esi)       #18.0
        movapd     256(%ebp,%edx,8), %xmm0                       #18.0
        movapd     272(%ebp,%edx,8), %xmm1                       #18.0
        movapd     288(%ebp,%edx,8), %xmm2                       #18.0
        movapd     304(%ebp,%edx,8), %xmm3                       #18.0
        movapd     320(%ebp,%edx,8), %xmm4                       #18.0
        movapd     336(%ebp,%edx,8), %xmm5                       #18.0
        movapd     352(%ebp,%edx,8), %xmm6                       #18.0
        movapd     368(%ebp,%edx,8), %xmm7                       #18.0
        movapd     %xmm0, ..1.transp_LOCAL.buf.6+256(%esi)       #18.0
        movapd     %xmm1, ..1.transp_LOCAL.buf.6+272(%esi)       #18.0
        movapd     %xmm2, ..1.transp_LOCAL.buf.6+288(%esi)       #18.0
        movapd     %xmm3, ..1.transp_LOCAL.buf.6+304(%esi)       #18.0
        movapd     %xmm4, ..1.transp_LOCAL.buf.6+320(%esi)       #18.0
        movapd     %xmm5, ..1.transp_LOCAL.buf.6+336(%esi)       #18.0
        movapd     %xmm6, ..1.transp_LOCAL.buf.6+352(%esi)       #18.0
        movapd     %xmm7, ..1.transp_LOCAL.buf.6+368(%esi)       #18.0
        movapd     384(%ebp,%edx,8), %xmm0                       #18.0
        movapd     400(%ebp,%edx,8), %xmm1                       #18.0
        movapd     416(%ebp,%edx,8), %xmm2                       #18.0
        movapd     432(%ebp,%edx,8), %xmm3                       #18.0
        movapd     448(%ebp,%edx,8), %xmm4                       #18.0
        movapd     464(%ebp,%edx,8), %xmm5                       #18.0
        movapd     480(%ebp,%edx,8), %xmm6                       #18.0
        movapd     496(%ebp,%edx,8), %xmm7                       #18.0
        movapd     %xmm0, ..1.transp_LOCAL.buf.6+384(%esi)       #18.0
        movapd     %xmm1, ..1.transp_LOCAL.buf.6+400(%esi)       #18.0
        movapd     %xmm2, ..1.transp_LOCAL.buf.6+416(%esi)       #18.0
        movapd     %xmm3, ..1.transp_LOCAL.buf.6+432(%esi)       #18.0
        movapd     %xmm4, ..1.transp_LOCAL.buf.6+448(%esi)       #18.0
        movapd     %xmm5, ..1.transp_LOCAL.buf.6+464(%esi)       #18.0
        movapd     %xmm6, ..1.transp_LOCAL.buf.6+480(%esi)       #18.0
        movapd     %xmm7, ..1.transp_LOCAL.buf.6+496(%esi)       #18.0
        addl      %ebx, %edi                                    #20.0
        addl      $512, %esi                                    #21.0
        cmpl      $15872, %esi                                  #21.0
        jle       .B1.6         # Prob 96%                      #21.0
                                # LOE eax ecx ebx ebp esi edi
.B1.7:                          # Preds .B1.6
        movl      24(%esp), %ebx                                #
        movl      12(%esp), %edx                                #
        movl      4(%esp), %ebp                                 #
        movl      %edx, 12(%esp)                                #22.0
        xorl      %esi, %esi                                    #22.0
        movl      64(%esp), %edx                                #22.0
        .align    4,0x90
                                # LOE eax edx ecx ebx ebp esi
.B1.8:                          # Preds .B1.8 .B1.7
        lea       (%ebx,%ebx), %edi                             #24.0
        movapd     ..1.transp_LOCAL.buf.6(%esi), %xmm0           #24.0
        movapd     ..1.transp_LOCAL.buf.6+512(%esi), %xmm1       #24.0
        movapd     ..1.transp_LOCAL.buf.6+1024(%esi), %xmm2      #24.0
        movapd     ..1.transp_LOCAL.buf.6+1536(%esi), %xmm3      #24.0
        movapd     ..1.transp_LOCAL.buf.6+2048(%esi), %xmm4      #24.0
        movapd     ..1.transp_LOCAL.buf.6+2560(%esi), %xmm5      #24.0
        movapd     ..1.transp_LOCAL.buf.6+3072(%esi), %xmm6      #24.0
        movapd     ..1.transp_LOCAL.buf.6+3584(%esi), %xmm7      #24.0
        movntpd     %xmm0, (%edx,%edi,8)                          #24.0
        movntpd     %xmm1, 16(%edx,%edi,8)                        #24.0
        movntpd     %xmm2, 32(%edx,%edi,8)                        #24.0
        movntpd     %xmm3, 48(%edx,%edi,8)                        #24.0
        movntpd     %xmm4, 64(%edx,%edi,8)                        #24.0
        movntpd     %xmm5, 80(%edx,%edi,8)                        #24.0
        movntpd     %xmm6, 96(%edx,%edi,8)                        #24.0
        movntpd     %xmm7, 112(%edx,%edi,8)                       #24.0
        movapd     ..1.transp_LOCAL.buf.6+4096(%esi), %xmm0      #24.0
        movapd     ..1.transp_LOCAL.buf.6+4608(%esi), %xmm1      #24.0
        movapd     ..1.transp_LOCAL.buf.6+5120(%esi), %xmm2      #24.0
        movapd     ..1.transp_LOCAL.buf.6+5632(%esi), %xmm3      #24.0
        movapd     ..1.transp_LOCAL.buf.6+6144(%esi), %xmm4      #24.0
        movapd     ..1.transp_LOCAL.buf.6+6656(%esi), %xmm5      #24.0
        movapd     ..1.transp_LOCAL.buf.6+7168(%esi), %xmm6      #24.0
        movapd     ..1.transp_LOCAL.buf.6+7680(%esi), %xmm7      #24.0
        movntpd     %xmm0, 128(%edx,%edi,8)                       #24.0
        movntpd     %xmm1, 144(%edx,%edi,8)                       #24.0
        movntpd     %xmm2, 160(%edx,%edi,8)                       #24.0
        movntpd     %xmm3, 176(%edx,%edi,8)                       #24.0
        movntpd     %xmm4, 192(%edx,%edi,8)                       #24.0
        movntpd     %xmm5, 208(%edx,%edi,8)                       #24.0
        movntpd     %xmm6, 224(%edx,%edi,8)                       #24.0
        movntpd     %xmm7, 240(%edx,%edi,8)                       #24.0
        movapd     ..1.transp_LOCAL.buf.6+8192(%esi), %xmm0      #24.0
        movapd     ..1.transp_LOCAL.buf.6+8704(%esi), %xmm1      #24.0
        movapd     ..1.transp_LOCAL.buf.6+9216(%esi), %xmm2      #24.0
        movapd     ..1.transp_LOCAL.buf.6+9728(%esi), %xmm3      #24.0
        movapd     ..1.transp_LOCAL.buf.6+10240(%esi), %xmm4     #24.0
        movapd     ..1.transp_LOCAL.buf.6+10752(%esi), %xmm5     #24.0
        movapd     ..1.transp_LOCAL.buf.6+11264(%esi), %xmm6     #24.0
        movapd     ..1.transp_LOCAL.buf.6+11776(%esi), %xmm7     #24.0
        movntpd     %xmm0, 256(%edx,%edi,8)                       #24.0
        movntpd     %xmm1, 272(%edx,%edi,8)                       #24.0
        movntpd     %xmm2, 288(%edx,%edi,8)                       #24.0
        movntpd     %xmm3, 304(%edx,%edi,8)                       #24.0
        movntpd     %xmm4, 320(%edx,%edi,8)                       #24.0
        movntpd     %xmm5, 336(%edx,%edi,8)                       #24.0
        movntpd     %xmm6, 352(%edx,%edi,8)                       #24.0
        movntpd     %xmm7, 368(%edx,%edi,8)                       #24.0
        movapd     ..1.transp_LOCAL.buf.6+12288(%esi), %xmm0     #24.0
        movapd     ..1.transp_LOCAL.buf.6+12800(%esi), %xmm1     #24.0
        movapd     ..1.transp_LOCAL.buf.6+13312(%esi), %xmm2     #24.0
        movapd     ..1.transp_LOCAL.buf.6+13824(%esi), %xmm3     #24.0
        movapd     ..1.transp_LOCAL.buf.6+14336(%esi), %xmm4     #24.0
        movapd     ..1.transp_LOCAL.buf.6+14848(%esi), %xmm5     #24.0
        movapd     ..1.transp_LOCAL.buf.6+15360(%esi), %xmm6     #24.0
        movapd     ..1.transp_LOCAL.buf.6+15872(%esi), %xmm7     #24.0
        movntpd     %xmm0, 384(%edx,%edi,8)                       #24.0
        movntpd     %xmm1, 400(%edx,%edi,8)                       #24.0
        movntpd     %xmm2, 416(%edx,%edi,8)                       #24.0
        movntpd     %xmm3, 432(%edx,%edi,8)                       #24.0
        movntpd     %xmm4, 448(%edx,%edi,8)                       #24.0
        movntpd     %xmm5, 464(%edx,%edi,8)                       #24.0
        movntpd     %xmm6, 480(%edx,%edi,8)                       #24.0
        movntpd     %xmm7, 496(%edx,%edi,8)                       #24.0
        addl      %ebp, %ebx                                    #26.0
        addl      $16, %esi                                     #27.0
        cmpl      $496, %esi                                    #27.0
        jle       .B1.8         # Prob 96%                      #27.0
                                # LOE eax edx ecx ebx ebp esi
.B1.9:                          # Preds .B1.8
        movl      12(%esp), %edx                                #
        addl      $32, %ecx                                     #28.0
        cmpl      %edx, %ecx                                    #28.0
        jle       .B1.5         # Prob 90%                      #28.0
                                # LOE eax edx ecx ebx ebp dl dh
.B1.10:                         # Preds .B1.9
        movl      20(%esp), %ebp                                #
        movl      16(%esp), %esi                                #
        movl      8(%esp), %edi                                 #
                                # LOE eax edx ebp esi edi
.B1.11:                         # Preds .B1.3 .B1.10
        addl      %esi, %eax                                    #29.0
        addl      $32, %ebp                                     #30.0
        cmpl      %edi, %ebp                                    #30.0
        jle       .B1.3         # Prob 90%                      #30.0
                                # LOE eax edx ebp esi edi
.B1.13:                         # Preds .B1.11 .B1.1
        addl      $32, %esp                                     #31.0
        popl      %ebx                                          #31.0
        popl      %ebp                                          #31.0
        popl      %esi                                          #31.0
        popl      %edi                                          #31.0
        ret                                                     #31.0
        .align    4,0x90
                                # LOE
# mark_end;
	.type	transp_,@function
	.size	transp_,.-transp_
	.bss
..1.transp_LOCAL.buf.6:
	.align  16, 0x00
	.type	..1.transp_LOCAL.buf.6,@object
	.size	..1.transp_LOCAL.buf.6,16384
	.space 16384	# pad
	.data
# -- End  transp_
	.data
EPC.RZERO.0:
	.long 0x00000000,0x00000000,0x00000000,0x00000000	# xf128
	.type	EPC.RZERO.0,@object
	.size	EPC.RZERO.0,16
	.data
# End

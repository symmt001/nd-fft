# as --gstabs --gdwarf2 -o transp_0.o transp.s ; make ndzfft1d ; ndzfft1d

# -- Machine type PW
# mark_description "Intel(R) Fortran Compiler for 32-bit applications, Version 6.0   Build 020722Z";
# mark_description "-Xlinker -rpath -Xlinker /opt/intel/compiler60/ia32/lib -S -xW -Zp16 -align";
	.ident "Intel(R) Fortran Compiler for 32-bit applications, Version 6.0   Build 020722Z"
	.ident "-Xlinker -rpath -Xlinker /opt/intel/compiler60/ia32/lib -S -xW -Zp16 -align"
	.file "transp_2.f"
	.text
	.data
	.align 4
	.bss
	.align 4
	.ident "-?comment:Intel(R) Fortran Compiler for 32-bit applications, Version 6.0   Build 020722Z  : transp_2.f : -Xlinker -rpath -Xlinker /opt/intel/compiler60/ia32/lib -S -xW -Zp16 -align"
	.data
	.text
# -- Begin  transp_
# mark_begin;
       .align    4,0x90
# parameter 1: 56 + %esp
# parameter 2: 60 + %esp
# parameter 3: 64 + %esp
# parameter 4: 68 + %esp
	.globl   transp_
transp_:
.B1.1:                          # Preds .B1.0
        pushl     %edi                                          #5.0
        pushl     %esi                                          #5.0
        pushl     %ebp                                          #5.0
        pushl     %ebx                                          #5.0
        subl      $36, %esp                                     #5.0
        movl      56(%esp), %eax                                #5.0
        xorl      %ebx, %ebx                                    #11.0
        movl      60(%esp), %edx                                #5.0
        movl      68(%esp), %ebp                                #5.0
        movl      (%edx), %esi                                  #11.0
        lea       -1(%esi), %edx                                #11.0
        testl     %edx, %edx                                    #11.0
        jl        .B1.19        # Prob 2%                       #11.0
                                # LOE eax edx ebx ebp esi
.B1.2:                          # Preds .B1.1
        movl      (%eax), %edi                                  #12.0
        lea       -1(%edi), %ecx                                #12.0
        addl      %edi, %edi                                    #15.0
        addl      %edi, %edi                                    #15.0
        addl      %edi, %edi                                    #15.0
        addl      %edi, %edi                                    #15.0
        movl      %edi, 24(%esp)                                #27.0
        addl      %esi, %esi                                    #27.0
        addl      %esi, %esi                                    #27.0
        addl      %esi, %esi                                    #27.0
        addl      %esi, %esi                                    #27.0
        lea       (%ebx,%ebx), %eax                             #27.0
        movl      %esi, 16(%esp)                                #27.0
        addl      %eax, %eax                                    #27.0
        addl      %eax, %eax                                    #27.0
        addl      %eax, %eax                                    #27.0
                                # LOE eax edx ecx ebx ebp
.B1.3:                          # Preds .B1.2 .B1.17
        xorl      %edi, %edi                                    #12.0
        testl     %ecx, %ecx                                    #12.0
        jl        .B1.17        # Prob 2%                       #12.0
                                # LOE eax edx ecx ebx edi
.B1.4:                          # Preds .B1.3
        movl      68(%esp), %ebp                                #
        movl      %eax, 20(%esp)                                #
        movl      %edx, 4(%esp)                                 #
        lea       (%ebp,%eax), %esi                             #
        lea       (%edi,%edi), %ebp                             #
        addl      %ebp, %ebp                                    #
        addl      %ebp, %ebp                                    #
        addl      %ebp, %ebp                                    #
        addl      64(%esp), %ebp                                #
                                # LOE ecx ebx ebp esi edi
.B1.5:                          # Preds .B1.4 .B1.15
        movl      %ecx, 12(%esp)                                #13.0
        movl      %ebx, %edx                                    #13.0
        movl      %ebx, 8(%esp)                                 #13.0
        movl      24(%esp), %ebx                                #13.0
        movl      %esi, 32(%esp)                                #18.0
		xorl	  %eax,%eax
		lea  ..1.transp_LOCAL.buf.6(%eax), %esi
#		addl      $15,  %esi
#		andl      $0xfffffff0, %esi
        .align    4,0x90
                                # LOE eax edx ebx ebp esi edi
.B1.6:                          # Preds .B1.6 .B1.5
        movl      %edx, %ecx                                    #15.0
        imull     %ebx, %ecx                                    #15.0
        movapd     (%ecx,%ebp), %xmm0                            #15.0
        movapd     16(%ecx,%ebp), %xmm1                          #15.0
        movapd     32(%ecx,%ebp), %xmm2                          #15.0
        movapd     48(%ecx,%ebp), %xmm3                          #15.0
        movapd     64(%ecx,%ebp), %xmm4                          #15.0
        movapd     80(%ecx,%ebp), %xmm5                          #15.0
        movapd     96(%ecx,%ebp), %xmm6                          #15.0
        movapd     112(%ecx,%ebp), %xmm7                         #15.0
        movapd     %xmm0,   (%esi)           #15.0
        movapd     %xmm1, 16(%esi)        #15.0
        movapd     %xmm2, 32(%esi)        #15.0
        movapd     %xmm3, 48(%esi)        #15.0
        movapd     %xmm4, 64(%esi)        #15.0
        movapd     %xmm5, 80(%esi)        #15.0
        movapd     %xmm6, 96(%esi)        #15.0
        movapd     %xmm7, 112(%esi)       #15.0
        movapd     128(%ecx,%ebp), %xmm0                         #15.0
        movapd     144(%ecx,%ebp), %xmm1                         #15.0
        movapd     160(%ecx,%ebp), %xmm2                         #15.0
        movapd     176(%ecx,%ebp), %xmm3                         #15.0
        movapd     192(%ecx,%ebp), %xmm4                         #15.0
        movapd     208(%ecx,%ebp), %xmm5                         #15.0
        movapd     224(%ecx,%ebp), %xmm6                         #15.0
        movapd     240(%ecx,%ebp), %xmm7                         #15.0
        movapd     %xmm0, 128(%esi)       #15.0
        movapd     %xmm1, 144(%esi)       #15.0
        movapd     %xmm2, 160(%esi)       #15.0
        movapd     %xmm3, 176(%esi)       #15.0
        movapd     %xmm4, 192(%esi)       #15.0
        movapd     %xmm5, 208(%esi)       #15.0
        movapd     %xmm6, 224(%esi)       #15.0
        movapd     %xmm7, 240(%esi)       #15.0
        movapd     256(%ecx,%ebp), %xmm0                         #15.0
        movapd     272(%ecx,%ebp), %xmm1                         #15.0
        movapd     288(%ecx,%ebp), %xmm2                         #15.0
        movapd     304(%ecx,%ebp), %xmm3                         #15.0
        movapd     320(%ecx,%ebp), %xmm4                         #15.0
        movapd     336(%ecx,%ebp), %xmm5                         #15.0
        movapd     352(%ecx,%ebp), %xmm6                         #15.0
        movapd     368(%ecx,%ebp), %xmm7                         #15.0
        movapd     %xmm0, 256(%esi)       #15.0
        movapd     %xmm1, 272(%esi)       #15.0
        movapd     %xmm2, 288(%esi)       #15.0
        movapd     %xmm3, 304(%esi)       #15.0
        movapd     %xmm4, 320(%esi)       #15.0
        movapd     %xmm5, 336(%esi)       #15.0
        movapd     %xmm6, 352(%esi)       #15.0
        movapd     %xmm7, 368(%esi)       #15.0
        movapd     384(%ecx,%ebp), %xmm0                         #15.0
        movapd     400(%ecx,%ebp), %xmm1                         #15.0
        movapd     416(%ecx,%ebp), %xmm2                         #15.0
        movapd     432(%ecx,%ebp), %xmm3                         #15.0
        movapd     448(%ecx,%ebp), %xmm4                         #15.0
        movapd     464(%ecx,%ebp), %xmm5                         #15.0
        movapd     480(%ecx,%ebp), %xmm6                         #15.0
        movapd     496(%ecx,%ebp), %xmm7                         #15.0
        movapd     %xmm0, 384(%esi)       #15.0
        movapd     %xmm1, 400(%esi)       #15.0
        movapd     %xmm2, 416(%esi)       #15.0
        movapd     %xmm3, 432(%esi)       #15.0
        movapd     %xmm4, 448(%esi)       #15.0
        movapd     %xmm5, 464(%esi)       #15.0
        movapd     %xmm6, 480(%esi)       #15.0
        movapd     %xmm7, 496(%esi)       #15.0
        addl      $1, %edx                                      #17.0
		addl      $512, %esi
        addl      $512, %eax                                    #17.0
        cmpl      $15872, %eax                                  #17.0
        jle       .B1.6         # Prob 96%                      #17.0
                                # LOE eax edx ebx ebp esi edi
.B1.7:                          # Preds .B1.6
        movl      %edi, 28(%esp)                                #18.0
        xorl      %eax, %eax                                    #18.0
        .align    4,0x90
                                # LOE eax ebp
.B1.8:                          # Preds .B1.12 .B1.7
        lea       1(%eax), %edx                                 #19.0
        movl      %edx, %ecx                                    #19.0
        cmpl      $31, %edx                                     #19.0
        jg        .B1.12        # Prob 10%                      #19.0
                                # LOE eax edx ecx ebp
.B1.9:                          # Preds .B1.8
        movl      %eax, %ebx                                    #
        shll      $9, %ebx                                      #
        addl      %eax, %eax                                    #
		movl      %ecx, %edi
		addl      %edi, %edi
		addl      %edi, %edi
		addl      %edi, %edi
		addl      %edi, %edi
        movl      %edi, %esi                                    #
		addl      %edi, %edi
		addl      %edi, %edi
		addl      %edi, %edi
		addl      %edi, %edi
		addl      %edi, %edi
		lea  ..1.transp_LOCAL.buf.6(%esi), %esi
		lea  ..1.transp_LOCAL.buf.6(%edi), %edi
        .align    4,0x90
                                # LOE eax edx ecx ebx ebp esi edi
.B1.10:                         # Preds .B1.9 .B1.10
        movapd     (%esi,%ebx), %xmm0      #20.0
        movapd     (%edi,%eax,8), %xmm1    #21.0
        movapd     %xmm1, (%esi,%ebx)      #21.0
        movapd     %xmm0, (%edi,%eax,8)    #22.0
        addl      $16, %esi                                     #23.0
        addl      $512, %edi                                    #23.0
        addl      $1, %ecx                                      #23.0
        cmpl      $31, %ecx                                     #23.0
        jle       .B1.10        # Prob 97%                      #23.0
                                # LOE eax edx ecx ebx ebp esi edi
.B1.12:                         # Preds .B1.10 .B1.8
        movl      %edx, %eax                                    #19.0
        cmpl      $30, %edx                                     #24.0
        jle       .B1.8         # Prob 96%                      #24.0
                                # LOE eax ebp
.B1.13:                         # Preds .B1.12
        movl      32(%esp), %esi                                #
        movl      28(%esp), %edi                                #
        movl      12(%esp), %ecx                                #
        movl      8(%esp), %ebx                                 #
        xorl      %edx, %edx                                    #25.0
        movl      %edi, %eax                                    #25.0
        movl      %ecx, 12(%esp)                                #25.0
        movl      %ebx, 8(%esp)                                 #25.0
        movl      16(%esp), %ebx                                #25.0
		pushl     %edi
		lea ..1.transp_LOCAL.buf.6(%edx), %edi
        .align    4,0x90
                                # LOE eax edx ebx ebp esi edi
.B1.14:                         # Preds .B1.14 .B1.13
        movapd    (%edi), %xmm0           #27.0
        movapd     16(%edi), %xmm1        #27.0
        movl      %eax, %ecx                                    #27.0
        imull     %ebx, %ecx                                    #27.0
        movapd     32(%edi), %xmm2        #27.0
        movapd     48(%edi), %xmm3        #27.0
        movapd     64(%edi), %xmm4        #27.0
        movapd     80(%edi), %xmm5        #27.0
        movapd     96(%edi), %xmm6        #27.0
        movapd     112(%edi), %xmm7       #27.0
        movntpd     %xmm0, (%ecx,%esi)                            #27.0
        movntpd     %xmm1, 16(%ecx,%esi)                          #27.0
        movntpd     %xmm2, 32(%ecx,%esi)                          #27.0
        movntpd     %xmm3, 48(%ecx,%esi)                          #27.0
        movntpd     %xmm4, 64(%ecx,%esi)                          #27.0
        movntpd     %xmm5, 80(%ecx,%esi)                          #27.0
        movntpd     %xmm6, 96(%ecx,%esi)                          #27.0
        movntpd     %xmm7, 112(%ecx,%esi)                         #27.0
        movapd     128(%edi), %xmm0       #27.0
        movapd     144(%edi), %xmm1       #27.0
        movapd     160(%edi), %xmm2       #27.0
        movapd     176(%edi), %xmm3       #27.0
        movapd     192(%edi), %xmm4       #27.0
        movapd     208(%edi), %xmm5       #27.0
        movapd     224(%edi), %xmm6       #27.0
        movapd     240(%edi), %xmm7       #27.0
        movntpd     %xmm0, 128(%ecx,%esi)                         #27.0
        movntpd     %xmm1, 144(%ecx,%esi)                         #27.0
        movntpd     %xmm2, 160(%ecx,%esi)                         #27.0
        movntpd     %xmm3, 176(%ecx,%esi)                         #27.0
        movntpd     %xmm4, 192(%ecx,%esi)                         #27.0
        movntpd     %xmm5, 208(%ecx,%esi)                         #27.0
        movntpd     %xmm6, 224(%ecx,%esi)                         #27.0
        movntpd     %xmm7, 240(%ecx,%esi)                         #27.0
        movapd     256(%edi), %xmm0       #27.0
        movapd     272(%edi), %xmm1       #27.0
        movapd     288(%edi), %xmm2       #27.0
        movapd     304(%edi), %xmm3       #27.0
        movapd     320(%edi), %xmm4       #27.0
        movapd     336(%edi), %xmm5       #27.0
        movapd     352(%edi), %xmm6       #27.0
        movapd     368(%edi), %xmm7       #27.0
        movntpd     %xmm0, 256(%ecx,%esi)                         #27.0
        movntpd     %xmm1, 272(%ecx,%esi)                         #27.0
        movntpd     %xmm2, 288(%ecx,%esi)                         #27.0
        movntpd     %xmm3, 304(%ecx,%esi)                         #27.0
        movntpd     %xmm4, 320(%ecx,%esi)                         #27.0
        movntpd     %xmm5, 336(%ecx,%esi)                         #27.0
        movntpd     %xmm6, 352(%ecx,%esi)                         #27.0
        movntpd     %xmm7, 368(%ecx,%esi)                         #27.0
        movapd     384(%edi), %xmm0       #27.0
        movapd     400(%edi), %xmm1       #27.0
        movapd     416(%edi), %xmm2       #27.0
        movapd     432(%edi), %xmm3       #27.0
        movapd     448(%edi), %xmm4       #27.0
        movapd     464(%edi), %xmm5       #27.0
        movapd     480(%edi), %xmm6       #27.0
        movapd     496(%edi), %xmm7       #27.0
        movntpd     %xmm0, 384(%ecx,%esi)                         #27.0
        movntpd     %xmm1, 400(%ecx,%esi)                         #27.0
        movntpd     %xmm2, 416(%ecx,%esi)                         #27.0
        movntpd     %xmm3, 432(%ecx,%esi)                         #27.0
        movntpd     %xmm4, 448(%ecx,%esi)                         #27.0
        movntpd     %xmm5, 464(%ecx,%esi)                         #27.0
        movntpd     %xmm6, 480(%ecx,%esi)                         #27.0
        movntpd     %xmm7, 496(%ecx,%esi)                         #27.0
        addl      $512, %edi                                    #29.0
        addl      $512, %edx                                    #29.0
        addl      $1, %eax                                      #29.0
        cmpl      $15872, %edx                                  #29.0
        jle       .B1.14        # Prob 96%                      #29.0
                                # LOE eax edx ebx ebp esi edi
.B1.15:                         # Preds .B1.14
		popl      %edi
        movl      12(%esp), %ecx                                #
        movl      8(%esp), %ebx                                 #
        addl      $512, %ebp                                    #30.0
        addl      $32, %edi                                     #30.0
        cmpl      %ecx, %edi                                    #30.0
        jle       .B1.5         # Prob 90%                      #30.0
                                # LOE ecx ebx ebp esi edi cl ch bl bh
.B1.16:                         # Preds .B1.15
        movl      20(%esp), %eax                                #
        movl      4(%esp), %edx                                 #
                                # LOE eax edx ecx ebx
.B1.17:                         # Preds .B1.3 .B1.16
        addl      $512, %eax                                    #31.0
        addl      $32, %ebx                                     #31.0
        cmpl      %edx, %ebx                                    #31.0
        jle       .B1.3         # Prob 90%                      #31.0
                                # LOE eax edx ecx ebx
.B1.19:                         # Preds .B1.17 .B1.1
        addl      $36, %esp                                     #32.0
        popl      %ebx                                          #32.0
        popl      %ebp                                          #32.0
        popl      %esi                                          #32.0
        popl      %edi                                          #32.0
        ret                                                     #32.0
        .align    4,0x90
                                # LOE
# mark_end;
	.type	transp_,@function
	.size	transp_,.-transp_
	.bss
..1.transp_LOCAL.buf.6:
	.align    16,0x90
	.type	..1.transp_LOCAL.buf.6,@object
	.size	..1.transp_LOCAL.buf.6,16384
	.space 16384 # pad
	.data
# -- End  transp_
	.data
EPC.RZERO.0:
	.long 0x00000000,0x00000000,0x00000000,0x00000000	# xf128
	.type	EPC.RZERO.0,@object
	.size	EPC.RZERO.0,16
	.data
# End

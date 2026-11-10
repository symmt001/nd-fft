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
		shll      $4, %ecx
        movl      %ecx, 4(%esp)                                 #29.0
        lea       -32(%ebx), %edx                               #14.0
        lea       (%ebx,%ebx), %esi                             #29.0
        addl      %esi, %esi                                    #29.0
        addl      %esi, %esi                                    #29.0
        addl      %esi, %esi                                    #29.0
        movl      %esi, 28(%esp)                                #29.0
        addl      %esi, %esi                                    #29.0
                                # LOE eax edx ebp esi edi
.B1.3:                          # Preds .B1.2 .B1.11
        movl      %ebp, %ebx                                    #13.0
		shll      $4, %ebx
        xorl      %ecx, %ecx                                    #14.0
        testl     %edx, %edx                                    #14.0
        jl        .B1.11        # Prob 2%                       #14.0
                                # LOE eax edx ecx ebx ebp esi edi
.B1.4:                          # Preds .B1.3
        movl      %ebp, 20(%esp)                                #
        movl      %esi, 16(%esp)                                #
        movl      %edi, 8(%esp)                                 #
        movl      4(%esp), %ebp                                 #
                                # LOE eax edx ecx ebx ebp
.B1.5:                          # Preds .B1.4 .B1.9
        lea       (%eax,%ecx), %edi                             #15.0
		shll      $4, %edi
		prefetchnta (%edi)
        xorl      %esi, %esi                                    #16.0
#		prefetcht0 (%esi)
        movl      %edx, 12(%esp)                                #16.0
        movl      %ebx, 24(%esp)                                #16.0
        movl      28(%esp), %ebx                                #16.0
        addl      60(%esp), %edi                                #16.0
        movl      64(%esp), %edx                                #22.0
        .align    4,0x90
                                # LOE eax ecx ebx ebp esi edi
.B1.6:                          # Preds .B1.6 .B1.5
        movapd     (%edi), %xmm0                            #15.0
        movapd     16(%edi), %xmm1                          #15.0
        movapd     32(%edi), %xmm2                          #15.0
        movapd     48(%edi), %xmm3                          #15.0
        movapd     64(%edi), %xmm4                          #15.0
        movapd     80(%edi), %xmm5                          #15.0
        movapd     96(%edi), %xmm6                          #15.0
        movapd     112(%edi), %xmm7                         #15.0
#		prefetchnta 128(%edi)
        movapd     %xmm0, ..1.transp_LOCAL.buf.6(%esi)           #15.0
        movapd     %xmm1, ..1.transp_LOCAL.buf.6+16(%esi)        #15.0
        movapd     %xmm2, ..1.transp_LOCAL.buf.6+32(%esi)        #15.0
        movapd     %xmm3, ..1.transp_LOCAL.buf.6+48(%esi)        #15.0
        movapd     %xmm4, ..1.transp_LOCAL.buf.6+64(%esi)        #15.0
        movapd     %xmm5, ..1.transp_LOCAL.buf.6+80(%esi)        #15.0
        movapd     %xmm6, ..1.transp_LOCAL.buf.6+96(%esi)        #15.0
        movapd     %xmm7, ..1.transp_LOCAL.buf.6+112(%esi)       #15.0
#		prefetcht0 128(%esi)
        movapd     128(%edi), %xmm0                         #15.0
        movapd     144(%edi), %xmm1                         #15.0
        movapd     160(%edi), %xmm2                         #15.0
        movapd     176(%edi), %xmm3                         #15.0
        movapd     192(%edi), %xmm4                         #15.0
        movapd     208(%edi), %xmm5                         #15.0
        movapd     224(%edi), %xmm6                         #15.0
        movapd     240(%edi), %xmm7                         #15.0
#		prefetchnta 256(%edi)
        movapd     %xmm0, ..1.transp_LOCAL.buf.6+128(%esi)       #15.0
        movapd     %xmm1, ..1.transp_LOCAL.buf.6+144(%esi)       #15.0
        movapd     %xmm2, ..1.transp_LOCAL.buf.6+160(%esi)       #15.0
        movapd     %xmm3, ..1.transp_LOCAL.buf.6+176(%esi)       #15.0
        movapd     %xmm4, ..1.transp_LOCAL.buf.6+192(%esi)       #15.0
        movapd     %xmm5, ..1.transp_LOCAL.buf.6+208(%esi)       #15.0
        movapd     %xmm6, ..1.transp_LOCAL.buf.6+224(%esi)       #15.0
        movapd     %xmm7, ..1.transp_LOCAL.buf.6+240(%esi)       #15.0
#		prefetcht0 256(%esi)
        movapd     256(%edi), %xmm0                         #15.0
        movapd     272(%edi), %xmm1                         #15.0
        movapd     288(%edi), %xmm2                         #15.0
        movapd     304(%edi), %xmm3                         #15.0
        movapd     320(%edi), %xmm4                         #15.0
        movapd     336(%edi), %xmm5                         #15.0
        movapd     352(%edi), %xmm6                         #15.0
        movapd     368(%edi), %xmm7                         #15.0
#		prefetchnta 384(%edi)
        movapd     %xmm0, ..1.transp_LOCAL.buf.6+256(%esi)       #15.0
        movapd     %xmm1, ..1.transp_LOCAL.buf.6+272(%esi)       #15.0
        movapd     %xmm2, ..1.transp_LOCAL.buf.6+288(%esi)       #15.0
        movapd     %xmm3, ..1.transp_LOCAL.buf.6+304(%esi)       #15.0
        movapd     %xmm4, ..1.transp_LOCAL.buf.6+320(%esi)       #15.0
        movapd     %xmm5, ..1.transp_LOCAL.buf.6+336(%esi)       #15.0
        movapd     %xmm6, ..1.transp_LOCAL.buf.6+352(%esi)       #15.0
        movapd     %xmm7, ..1.transp_LOCAL.buf.6+368(%esi)       #15.0
#		prefetcht0 384(%esi)
        movapd     384(%edi), %xmm0                         #15.0
        movapd     400(%edi), %xmm1                         #15.0
        movapd     416(%edi), %xmm2                         #15.0
        movapd     432(%edi), %xmm3                         #15.0
        movapd     448(%edi), %xmm4                         #15.0
        movapd     464(%edi), %xmm5                         #15.0
        movapd     480(%edi), %xmm6                         #15.0
        movapd     496(%edi), %xmm7                         #15.0
#		prefetchnta (%edi,%ebx)
        movapd     %xmm0, ..1.transp_LOCAL.buf.6+384(%esi)       #15.0
        movapd     %xmm1, ..1.transp_LOCAL.buf.6+400(%esi)       #15.0
        movapd     %xmm2, ..1.transp_LOCAL.buf.6+416(%esi)       #15.0
        movapd     %xmm3, ..1.transp_LOCAL.buf.6+432(%esi)       #15.0
        movapd     %xmm4, ..1.transp_LOCAL.buf.6+448(%esi)       #15.0
        movapd     %xmm5, ..1.transp_LOCAL.buf.6+464(%esi)       #15.0
        movapd     %xmm6, ..1.transp_LOCAL.buf.6+480(%esi)       #15.0
        movapd     %xmm7, ..1.transp_LOCAL.buf.6+496(%esi)       #15.0
#		prefetcht0 512(%esi)
        addl      %ebx, %edi                                    #20.0
        addl      $512, %esi                                    #21.0
        cmpl      $15872, %esi                                  #21.0
        jle       .B1.6         # Prob 96%                      #21.0
                                # LOE eax ecx ebx ebp esi edi
.B1.7:                          # Preds .B1.6
        movl      24(%esp), %ebx                                #
        xorl      %esi, %esi                                    #22.0
        .align    4,0x90
                                # LOE eax edx ecx ebx ebp esi
.B1.8:                          # Preds .B1.8 .B1.7
        movapd     ..1.transp_LOCAL.buf.6(%esi), %xmm0           #20.0
        movapd     ..1.transp_LOCAL.buf.6+512(%esi), %xmm1       #20.0
        movapd     ..1.transp_LOCAL.buf.6+1024(%esi), %xmm2      #20.0
        movapd     ..1.transp_LOCAL.buf.6+1536(%esi), %xmm3      #20.0
        movapd     ..1.transp_LOCAL.buf.6+2048(%esi), %xmm4      #20.0
        movapd     ..1.transp_LOCAL.buf.6+2560(%esi), %xmm5      #20.0
        movapd     ..1.transp_LOCAL.buf.6+3072(%esi), %xmm6      #20.0
        movapd     ..1.transp_LOCAL.buf.6+3584(%esi), %xmm7      #20.0
        movntpd     %xmm0, (%edx,%ebx)                            #20.0
        movntpd     %xmm1, 16(%edx,%ebx)                          #20.0
        movntpd     %xmm2, 32(%edx,%ebx)                          #20.0
        movntpd     %xmm3, 48(%edx,%ebx)                          #20.0
        movntpd     %xmm4, 64(%edx,%ebx)                          #20.0
        movntpd     %xmm5, 80(%edx,%ebx)                          #20.0
        movntpd     %xmm6, 96(%edx,%ebx)                          #20.0
        movntpd     %xmm7, 112(%edx,%ebx)                         #20.0
        movapd     ..1.transp_LOCAL.buf.6+4096(%esi), %xmm0      #20.0
        movapd     ..1.transp_LOCAL.buf.6+4608(%esi), %xmm1      #20.0
        movapd     ..1.transp_LOCAL.buf.6+5120(%esi), %xmm2      #20.0
        movapd     ..1.transp_LOCAL.buf.6+5632(%esi), %xmm3      #20.0
        movapd     ..1.transp_LOCAL.buf.6+6144(%esi), %xmm4      #20.0
        movapd     ..1.transp_LOCAL.buf.6+6656(%esi), %xmm5      #20.0
        movapd     ..1.transp_LOCAL.buf.6+7168(%esi), %xmm6      #20.0
        movapd     ..1.transp_LOCAL.buf.6+7680(%esi), %xmm7      #20.0
        movntpd     %xmm0, 128(%edx,%ebx)                         #20.0
        movntpd     %xmm1, 144(%edx,%ebx)                         #20.0
        movntpd     %xmm2, 160(%edx,%ebx)                         #20.0
        movntpd     %xmm3, 176(%edx,%ebx)                         #20.0
        movntpd     %xmm4, 192(%edx,%ebx)                         #20.0
        movntpd     %xmm5, 208(%edx,%ebx)                         #20.0
        movntpd     %xmm6, 224(%edx,%ebx)                         #20.0
        movntpd     %xmm7, 240(%edx,%ebx)                         #20.0
        movapd     ..1.transp_LOCAL.buf.6+8192(%esi), %xmm0      #20.0
        movapd     ..1.transp_LOCAL.buf.6+8704(%esi), %xmm1      #20.0
        movapd     ..1.transp_LOCAL.buf.6+9216(%esi), %xmm2      #20.0
        movapd     ..1.transp_LOCAL.buf.6+9728(%esi), %xmm3      #20.0
        movapd     ..1.transp_LOCAL.buf.6+10240(%esi), %xmm4     #20.0
        movapd     ..1.transp_LOCAL.buf.6+10752(%esi), %xmm5     #20.0
        movapd     ..1.transp_LOCAL.buf.6+11264(%esi), %xmm6     #20.0
        movapd     ..1.transp_LOCAL.buf.6+11776(%esi), %xmm7     #20.0
        movntpd     %xmm0, 256(%edx,%ebx)                         #20.0
        movntpd     %xmm1, 272(%edx,%ebx)                         #20.0
        movntpd     %xmm2, 288(%edx,%ebx)                         #20.0
        movntpd     %xmm3, 304(%edx,%ebx)                         #20.0
        movntpd     %xmm4, 320(%edx,%ebx)                         #20.0
        movntpd     %xmm5, 336(%edx,%ebx)                         #20.0
        movntpd     %xmm6, 352(%edx,%ebx)                         #20.0
        movntpd     %xmm7, 368(%edx,%ebx)                         #20.0
        movapd     ..1.transp_LOCAL.buf.6+12288(%esi), %xmm0     #20.0
        movapd     ..1.transp_LOCAL.buf.6+12800(%esi), %xmm1     #20.0
        movapd     ..1.transp_LOCAL.buf.6+13312(%esi), %xmm2     #20.0
        movapd     ..1.transp_LOCAL.buf.6+13824(%esi), %xmm3     #20.0
        movapd     ..1.transp_LOCAL.buf.6+14336(%esi), %xmm4     #20.0
        movapd     ..1.transp_LOCAL.buf.6+14848(%esi), %xmm5     #20.0
        movapd     ..1.transp_LOCAL.buf.6+15360(%esi), %xmm6     #20.0
        movapd     ..1.transp_LOCAL.buf.6+15872(%esi), %xmm7     #20.0
        movntpd     %xmm0, 384(%edx,%ebx)                         #20.0
        movntpd     %xmm1, 400(%edx,%ebx)                         #20.0
        movntpd     %xmm2, 416(%edx,%ebx)                         #20.0
        movntpd     %xmm3, 432(%edx,%ebx)                         #20.0
        movntpd     %xmm4, 448(%edx,%ebx)                         #20.0
        movntpd     %xmm5, 464(%edx,%ebx)                         #20.0
        movntpd     %xmm6, 480(%edx,%ebx)                         #20.0
        movntpd     %xmm7, 496(%edx,%ebx)                         #20.0
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
	.align 16, 0x00
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

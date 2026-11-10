# as --gstabs --gdwarf2 -o transp_0.o transp1.s ; make ndzfft1d ; ndzfft1d
#
# ==============================================================================
# [Historical Background & Optimization Note / 開発秘話・最適化の背景]
#
# このアセンブリコードは、Fortran で書かれた 32x32 ブロック転置ルーチン (transp_2.f) を
# Intel Fortran Compiler 6.0 で `-S -xW` (IA-32 / Pentium 4 SSE2) を指定して出力させた後、
# 著者が Intel のアーキテクチャマニュアルと首っ引きになりながら手作業で編集したものです。
#
# 巨大配列の転置では、書き込み先 (dst) へのストアが L1/L2 キャッシュ容量を遥かに超えて
# キャッシュを spill (溢れ・汚染) させ、後続の FFT 演算に必要なデータを追い出してしまいます。
# そこで、コンパイラが出力した通常のストア命令を、キャッシュをバイパスして主記憶へ直接書き込む
# SSE2 のノンテンポラル・ストリーミングストア命令 `movntpd` へ手動で置換・最適化しました。
#
# This assembly was derived by compiling transp_2.f with Intel Fortran Compiler 6.0
# using `-S -xW` (IA-32 / Pentium 4 SSE2 target).
# The author manually modified the generated assembly by referencing the Intel manuals,
# replacing the memory store instructions with SSE2 non-temporal streaming stores (`movntpd`)
# to bypass cache pollution during large-scale matrix transpositions.
# ==============================================================================
#
# -- Machine type PW
# mark_description "Intel(R) Fortran Compiler for 32-bit applications, Version 6.0   Build 020722Z";
# mark_description "-Xlinker -rpath -Xlinker /opt/intel/compiler60/ia32/lib -S -xW";
	.ident "Intel(R) Fortran Compiler for 32-bit applications, Version 6.0   Build 020722Z"
	.ident "-Xlinker -rpath -Xlinker /opt/intel/compiler60/ia32/lib -S -xW"
	.file "transp_2.f"
	.text
	.data
	.align 4
	.bss
	.align 4
	.ident "-?comment:Intel(R) Fortran Compiler for 32-bit applications, Version 6.0   Build 020722Z  : transp_2.f : -Xlinker -rpath -Xlinker /opt/intel/compiler60/ia32/lib -S -xW"
	.data
	.text
# -- Begin  transp_
# mark_begin;
       .align    4,0x90
# parameter 1: 48 + %esp
# parameter 2: 52 + %esp
# parameter 3: 56 + %esp
# parameter 4: 60 + %esp
	.globl   transp_
transp_:
.B1.1:                          # Preds .B1.0
        pushl     %edi                                          #5.0
        pushl     %esi                                          #5.0
        pushl     %ebp                                          #5.0
        pushl     %ebx                                          #5.0
        subl      $28, %esp                                     #5.0
        movl      48(%esp), %eax                                #5.0
        xorl      %ebx, %ebx                                    #11.0
        movl      52(%esp), %edx                                #5.0
        movl      60(%esp), %ebp                                #5.0
        movl      (%edx), %esi                                  #11.0
        lea       -32(%esi), %edx                               #11.0
        testl     %edx, %edx                                    #11.0
        jl        .B1.13        # Prob 2%                       #11.0
                                # LOE eax edx ebx ebp esi
.B1.2:                          # Preds .B1.1
        movl      (%eax), %edi                                  #12.0
        lea       -32(%edi), %ecx                               #12.0
        addl      %edi, %edi                                    #15.0
        addl      %edi, %edi                                    #15.0
        addl      %edi, %edi                                    #15.0
        addl      %edi, %edi                                    #15.0
        movl      %edi, 24(%esp)                                #20.0
        addl      %esi, %esi                                    #20.0
        addl      %esi, %esi                                    #20.0
        addl      %esi, %esi                                    #20.0
        addl      %esi, %esi                                    #20.0
        lea       (%ebx,%ebx), %eax                             #20.0
        movl      %esi, 16(%esp)                                #20.0
        addl      %eax, %eax                                    #20.0
        addl      %eax, %eax                                    #20.0
        addl      %eax, %eax                                    #20.0
                                # LOE eax edx ecx ebx ebp
.B1.3:                          # Preds .B1.2 .B1.11
        xorl      %edi, %edi                                    #12.0
        testl     %ecx, %ecx                                    #12.0
        jl        .B1.11        # Prob 2%                       #12.0
                                # LOE eax edx ecx ebx edi
.B1.4:                          # Preds .B1.3
        movl      60(%esp), %ebp                                #
        movl      %eax, 20(%esp)                                #
        movl      %edx, 4(%esp)                                 #
        lea       (%ebp,%eax), %esi                             #
        lea       (%edi,%edi), %ebp                             #
        addl      %ebp, %ebp                                    #
        addl      %ebp, %ebp                                    #
        addl      %ebp, %ebp                                    #
        addl      56(%esp), %ebp                                #
                                # LOE ecx ebx ebp esi edi
.B1.5:                          # Preds .B1.4 .B1.9
        movl      %ecx, 12(%esp)                                #13.0
        movl      %ebx, %edx                                    #13.0
        movl      %ebx, 8(%esp)                                 #13.0
        movl      24(%esp), %ebx                                #13.0
        xorl      %eax, %eax                                    #13.0
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
        movapd     %xmm0, ..1.transp_LOCAL.buf.6(%eax)           #15.0
        movapd     %xmm1, ..1.transp_LOCAL.buf.6+16(%eax)        #15.0
        movapd     %xmm2, ..1.transp_LOCAL.buf.6+32(%eax)        #15.0
        movapd     %xmm3, ..1.transp_LOCAL.buf.6+48(%eax)        #15.0
        movapd     %xmm4, ..1.transp_LOCAL.buf.6+64(%eax)        #15.0
        movapd     %xmm5, ..1.transp_LOCAL.buf.6+80(%eax)        #15.0
        movapd     %xmm6, ..1.transp_LOCAL.buf.6+96(%eax)        #15.0
        movapd     %xmm7, ..1.transp_LOCAL.buf.6+112(%eax)       #15.0
        movapd     128(%ecx,%ebp), %xmm0                         #15.0
        movapd     144(%ecx,%ebp), %xmm1                         #15.0
        movapd     160(%ecx,%ebp), %xmm2                         #15.0
        movapd     176(%ecx,%ebp), %xmm3                         #15.0
        movapd     192(%ecx,%ebp), %xmm4                         #15.0
        movapd     208(%ecx,%ebp), %xmm5                         #15.0
        movapd     224(%ecx,%ebp), %xmm6                         #15.0
        movapd     240(%ecx,%ebp), %xmm7                         #15.0
        movapd     %xmm0, ..1.transp_LOCAL.buf.6+128(%eax)       #15.0
        movapd     %xmm1, ..1.transp_LOCAL.buf.6+144(%eax)       #15.0
        movapd     %xmm2, ..1.transp_LOCAL.buf.6+160(%eax)       #15.0
        movapd     %xmm3, ..1.transp_LOCAL.buf.6+176(%eax)       #15.0
        movapd     %xmm4, ..1.transp_LOCAL.buf.6+192(%eax)       #15.0
        movapd     %xmm5, ..1.transp_LOCAL.buf.6+208(%eax)       #15.0
        movapd     %xmm6, ..1.transp_LOCAL.buf.6+224(%eax)       #15.0
        movapd     %xmm7, ..1.transp_LOCAL.buf.6+240(%eax)       #15.0
        movapd     256(%ecx,%ebp), %xmm0                         #15.0
        movapd     272(%ecx,%ebp), %xmm1                         #15.0
        movapd     288(%ecx,%ebp), %xmm2                         #15.0
        movapd     304(%ecx,%ebp), %xmm3                         #15.0
        movapd     320(%ecx,%ebp), %xmm4                         #15.0
        movapd     336(%ecx,%ebp), %xmm5                         #15.0
        movapd     352(%ecx,%ebp), %xmm6                         #15.0
        movapd     368(%ecx,%ebp), %xmm7                         #15.0
        movapd     %xmm0, ..1.transp_LOCAL.buf.6+256(%eax)       #15.0
        movapd     %xmm1, ..1.transp_LOCAL.buf.6+272(%eax)       #15.0
        movapd     %xmm2, ..1.transp_LOCAL.buf.6+288(%eax)       #15.0
        movapd     %xmm3, ..1.transp_LOCAL.buf.6+304(%eax)       #15.0
        movapd     %xmm4, ..1.transp_LOCAL.buf.6+320(%eax)       #15.0
        movapd     %xmm5, ..1.transp_LOCAL.buf.6+336(%eax)       #15.0
        movapd     %xmm6, ..1.transp_LOCAL.buf.6+352(%eax)       #15.0
        movapd     %xmm7, ..1.transp_LOCAL.buf.6+368(%eax)       #15.0
        movapd     384(%ecx,%ebp), %xmm0                         #15.0
        movapd     400(%ecx,%ebp), %xmm1                         #15.0
        movapd     416(%ecx,%ebp), %xmm2                         #15.0
        movapd     432(%ecx,%ebp), %xmm3                         #15.0
        movapd     448(%ecx,%ebp), %xmm4                         #15.0
        movapd     464(%ecx,%ebp), %xmm5                         #15.0
        movapd     480(%ecx,%ebp), %xmm6                         #15.0
        movapd     496(%ecx,%ebp), %xmm7                         #15.0
        movapd     %xmm0, ..1.transp_LOCAL.buf.6+384(%eax)       #15.0
        movapd     %xmm1, ..1.transp_LOCAL.buf.6+400(%eax)       #15.0
        movapd     %xmm2, ..1.transp_LOCAL.buf.6+416(%eax)       #15.0
        movapd     %xmm3, ..1.transp_LOCAL.buf.6+432(%eax)       #15.0
        movapd     %xmm4, ..1.transp_LOCAL.buf.6+448(%eax)       #15.0
        movapd     %xmm5, ..1.transp_LOCAL.buf.6+464(%eax)       #15.0
        movapd     %xmm6, ..1.transp_LOCAL.buf.6+480(%eax)       #15.0
        movapd     %xmm7, ..1.transp_LOCAL.buf.6+496(%eax)       #15.0
        addl      $1, %edx                                      #17.0
        addl      $512, %eax                                    #17.0
        cmpl      $15872, %eax                                  #17.0
        jle       .B1.6         # Prob 96%                      #17.0
                                # LOE eax edx ebx ebp esi edi
.B1.7:                          # Preds .B1.6
        movl      12(%esp), %ecx                                #
        movl      8(%esp), %ebx                                 #
        movl      %ecx, 12(%esp)                                #18.0
        movl      16(%esp), %ecx                                #18.0
        movl      %ebx, 8(%esp)                                 #18.0
        xorl      %edx, %edx                                    #18.0
        movl      %edi, %eax                                    #18.0
        .align    4,0x90
                                # LOE eax edx ecx ebp esi edi
.B1.8:                          # Preds .B1.8 .B1.7
        movapd     ..1.transp_LOCAL.buf.6(%edx), %xmm0           #20.0
        movapd     ..1.transp_LOCAL.buf.6+512(%edx), %xmm1       #20.0
        movl      %eax, %ebx                                    #20.0
        imull     %ecx, %ebx                                    #20.0
        movapd     ..1.transp_LOCAL.buf.6+1024(%edx), %xmm2      #20.0
        movapd     ..1.transp_LOCAL.buf.6+1536(%edx), %xmm3      #20.0
        movapd     ..1.transp_LOCAL.buf.6+2048(%edx), %xmm4      #20.0
        movapd     ..1.transp_LOCAL.buf.6+2560(%edx), %xmm5      #20.0
        movapd     ..1.transp_LOCAL.buf.6+3072(%edx), %xmm6      #20.0
        movapd     ..1.transp_LOCAL.buf.6+3584(%edx), %xmm7      #20.0
        movntpd     %xmm0, (%ebx,%esi)                            #20.0
        movntpd     %xmm1, 16(%ebx,%esi)                          #20.0
        movntpd     %xmm2, 32(%ebx,%esi)                          #20.0
        movntpd     %xmm3, 48(%ebx,%esi)                          #20.0
        movntpd     %xmm4, 64(%ebx,%esi)                          #20.0
        movntpd     %xmm5, 80(%ebx,%esi)                          #20.0
        movntpd     %xmm6, 96(%ebx,%esi)                          #20.0
        movntpd     %xmm7, 112(%ebx,%esi)                         #20.0
        movapd     ..1.transp_LOCAL.buf.6+4096(%edx), %xmm0      #20.0
        movapd     ..1.transp_LOCAL.buf.6+4608(%edx), %xmm1      #20.0
        movapd     ..1.transp_LOCAL.buf.6+5120(%edx), %xmm2      #20.0
        movapd     ..1.transp_LOCAL.buf.6+5632(%edx), %xmm3      #20.0
        movapd     ..1.transp_LOCAL.buf.6+6144(%edx), %xmm4      #20.0
        movapd     ..1.transp_LOCAL.buf.6+6656(%edx), %xmm5      #20.0
        movapd     ..1.transp_LOCAL.buf.6+7168(%edx), %xmm6      #20.0
        movapd     ..1.transp_LOCAL.buf.6+7680(%edx), %xmm7      #20.0
        movntpd     %xmm0, 128(%ebx,%esi)                         #20.0
        movntpd     %xmm1, 144(%ebx,%esi)                         #20.0
        movntpd     %xmm2, 160(%ebx,%esi)                         #20.0
        movntpd     %xmm3, 176(%ebx,%esi)                         #20.0
        movntpd     %xmm4, 192(%ebx,%esi)                         #20.0
        movntpd     %xmm5, 208(%ebx,%esi)                         #20.0
        movntpd     %xmm6, 224(%ebx,%esi)                         #20.0
        movntpd     %xmm7, 240(%ebx,%esi)                         #20.0
        movapd     ..1.transp_LOCAL.buf.6+8192(%edx), %xmm0      #20.0
        movapd     ..1.transp_LOCAL.buf.6+8704(%edx), %xmm1      #20.0
        movapd     ..1.transp_LOCAL.buf.6+9216(%edx), %xmm2      #20.0
        movapd     ..1.transp_LOCAL.buf.6+9728(%edx), %xmm3      #20.0
        movapd     ..1.transp_LOCAL.buf.6+10240(%edx), %xmm4     #20.0
        movapd     ..1.transp_LOCAL.buf.6+10752(%edx), %xmm5     #20.0
        movapd     ..1.transp_LOCAL.buf.6+11264(%edx), %xmm6     #20.0
        movapd     ..1.transp_LOCAL.buf.6+11776(%edx), %xmm7     #20.0
        movntpd     %xmm0, 256(%ebx,%esi)                         #20.0
        movntpd     %xmm1, 272(%ebx,%esi)                         #20.0
        movntpd     %xmm2, 288(%ebx,%esi)                         #20.0
        movntpd     %xmm3, 304(%ebx,%esi)                         #20.0
        movntpd     %xmm4, 320(%ebx,%esi)                         #20.0
        movntpd     %xmm5, 336(%ebx,%esi)                         #20.0
        movntpd     %xmm6, 352(%ebx,%esi)                         #20.0
        movntpd     %xmm7, 368(%ebx,%esi)                         #20.0
        movapd     ..1.transp_LOCAL.buf.6+12288(%edx), %xmm0     #20.0
        movapd     ..1.transp_LOCAL.buf.6+12800(%edx), %xmm1     #20.0
        movapd     ..1.transp_LOCAL.buf.6+13312(%edx), %xmm2     #20.0
        movapd     ..1.transp_LOCAL.buf.6+13824(%edx), %xmm3     #20.0
        movapd     ..1.transp_LOCAL.buf.6+14336(%edx), %xmm4     #20.0
        movapd     ..1.transp_LOCAL.buf.6+14848(%edx), %xmm5     #20.0
        movapd     ..1.transp_LOCAL.buf.6+15360(%edx), %xmm6     #20.0
        movapd     ..1.transp_LOCAL.buf.6+15872(%edx), %xmm7     #20.0
        movntpd     %xmm0, 384(%ebx,%esi)                         #20.0
        movntpd     %xmm1, 400(%ebx,%esi)                         #20.0
        movntpd     %xmm2, 416(%ebx,%esi)                         #20.0
        movntpd     %xmm3, 432(%ebx,%esi)                         #20.0
        movntpd     %xmm4, 448(%ebx,%esi)                         #20.0
        movntpd     %xmm5, 464(%ebx,%esi)                         #20.0
        movntpd     %xmm6, 480(%ebx,%esi)                         #20.0
        movntpd     %xmm7, 496(%ebx,%esi)                         #20.0
        addl      $16, %edx                                     #22.0
        addl      $1, %eax                                      #22.0
        cmpl      $496, %edx                                    #22.0
        jle       .B1.8         # Prob 96%                      #22.0
                                # LOE eax edx ecx ebp esi edi
.B1.9:                          # Preds .B1.8
        movl      12(%esp), %ecx                                #
        movl      8(%esp), %ebx                                 #
        addl      $512, %ebp                                    #23.0
        addl      $32, %edi                                     #23.0
        cmpl      %ecx, %edi                                    #23.0
        jle       .B1.5         # Prob 90%                      #23.0
                                # LOE ecx ebx ebp esi edi cl ch bl bh
.B1.10:                         # Preds .B1.9
        movl      20(%esp), %eax                                #
        movl      4(%esp), %edx                                 #
                                # LOE eax edx ecx ebx
.B1.11:                         # Preds .B1.3 .B1.10
        addl      $512, %eax                                    #24.0
        addl      $32, %ebx                                     #24.0
        cmpl      %edx, %ebx                                    #24.0
        jle       .B1.3         # Prob 90%                      #24.0
                                # LOE eax edx ecx ebx
.B1.13:                         # Preds .B1.11 .B1.1
        addl      $28, %esp                                     #25.0
        popl      %ebx                                          #25.0
        popl      %ebp                                          #25.0
        popl      %esi                                          #25.0
        popl      %edi                                          #25.0
        ret                                                     #25.0
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

# -- Machine type PW
# mark_description "Intel(R) C++ Compiler for 32-bit applications, Version 6.0   Build 020722Z";
# mark_description "-long_double -S -xW";
	.ident "Intel(R) C++ Compiler for 32-bit applications, Version 6.0   Build 020722Z"
	.ident "-long_double -S -xW"
	.file "copy_test.c"
	.text
	.data
	.align 4
	.bss
	.align 4
	.ident "-?comment:Intel(R) C++ Compiler for 32-bit applications, Version 6.0   Build 020722Z  : copy_test.c : -long_double -S -xW"
	.data
	.text
# -- Begin  get_rt
# mark_begin;
       .align    4,0x90
	.globl   get_rt
get_rt:
.B1.1:                          # Preds .B1.0
        rdtsc 
	                                                        #7.0
        ret                                                     #11.1
        .align    4,0x90
                                # LOE
# mark_end;
	.type	get_rt,@function
	.size	get_rt,.-get_rt
	.data
# -- End  get_rt
	.data
	.text
# -- Begin  copy_
# mark_begin;
       .align    4,0x90
# parameter 1: 16 + %esp
# parameter 2: 20 + %esp
# parameter 3: 24 + %esp
	.globl   copy_
copy_:
.B2.1:                          # Preds .B2.0
        pushl     %edi                                          #20.20
        pushl     %esi                                          #20.20
        pushl     %ebp                                          #20.20
        movl      16(%esp), %ebp                                #18.6
        movl      20(%esp), %ecx                                #18.6
        movl      24(%esp), %edx                                #18.6
        movl      (%ebp), %esi                                  #22.17
        xorl      %eax, %eax                                    #22.7
        lea       -16(%esi), %edi                               #22.17
        testl     %edi, %edi                                    #22.3
        jl        .B2.5         # Prob 2%                       #22.3
        .align    4,0x90
                                # LOE eax edx ecx ebx ebp esi
.B2.3:                          # Preds .B2.1 .B2.3
        movapd     (%ecx,%eax,8), %xmm0                          #23.9
        movapd     16(%ecx,%eax,8), %xmm1                        #25.11
        movapd     32(%ecx,%eax,8), %xmm2                        #27.11
        movapd     48(%ecx,%eax,8), %xmm3                        #29.11
        movapd     64(%ecx,%eax,8), %xmm4                        #31.11
        movapd     80(%ecx,%eax,8), %xmm5                        #33.12
        movapd     96(%ecx,%eax,8), %xmm6                        #35.12
        movapd     112(%ecx,%eax,8), %xmm7                       #37.12
        movapd     %xmm0, (%edx,%eax,8)                          #23.2
        movapd     %xmm1, 16(%edx,%eax,8)                        #25.2
        movapd     %xmm2, 32(%edx,%eax,8)                        #27.2
        movapd     %xmm3, 48(%edx,%eax,8)                        #29.2
        movapd     %xmm4, 64(%edx,%eax,8)                        #31.2
        movapd     %xmm5, 80(%edx,%eax,8)                        #33.2
        movapd     %xmm6, 96(%edx,%eax,8)                        #35.2
        movapd     %xmm7, 112(%edx,%eax,8)                       #37.2
        addl      $16, %eax                                     #22.30
        movl      (%ebp), %esi                                  #22.17
        lea       -16(%esi), %edi                               #22.17
        cmpl      %edi, %eax                                    #22.3
        jle       .B2.3         # Prob 97%                      #22.3
                                # LOE eax edx ecx ebx ebp esi
.B2.5:                          # Preds .B2.3 .B2.1
        cmpl      %esi, %eax                                    #40.3
        jge       .B2.9         # Prob 2%                       #40.3
        .align    4,0x90
                                # LOE eax edx ecx ebx ebp
.B2.7:                          # Preds .B2.5 .B2.7
        movsd     (%ecx,%eax,8), %xmm0                          #41.9
        movsd     %xmm0, (%edx,%eax,8)                          #41.2
        addl      $1, %eax                                      #40.19
        cmpl      (%ebp), %eax                                  #40.3
        jl        .B2.7         # Prob 97%                      #40.3
                                # LOE eax edx ecx ebx ebp
.B2.9:                          # Preds .B2.7 .B2.5
        popl      %ebp                                          #43.1
        popl      %esi                                          #43.1
        popl      %edi                                          #43.1
        ret                                                     #43.1
        .align    4,0x90
                                # LOE
# mark_end;
	.type	copy_,@function
	.size	copy_,.-copy_
	.data
# -- End  copy_
	.data
_2il0floatpacket.7:
	.long 0x00000000,0x409f4000	# xf64
	.type	_2il0floatpacket.7,@object
	.size	_2il0floatpacket.7,8
_2il0floatpacket.5:
	.long 0x00000000,0x3ff00000	# xf64
	.type	_2il0floatpacket.5,@object
	.size	_2il0floatpacket.5,8
_2il0floatpacket.3:
	.long 0x00000000,0x413e8480	# xf64
	.type	_2il0floatpacket.3,@object
	.size	_2il0floatpacket.3,8
_2il0floatpacket.1:
	.long 0x9999999a,0x3fb99999	# xf64
	.type	_2il0floatpacket.1,@object
	.size	_2il0floatpacket.1,8
__STRING.1:
	.byte 37	# s8
	.byte 105	# s8
	.byte 44	# s8
	.byte 32	# s8
	.byte 37	# s8
	.byte 101	# s8
	.byte 10	# s8
	.byte 0	# s8
	.type	__STRING.1,@object
	.size	__STRING.1,8
__STRING.0:
	.byte 98	# s8
	.byte 108	# s8
	.byte 111	# s8
	.byte 99	# s8
	.byte 107	# s8
	.byte 32	# s8
	.byte 115	# s8
	.byte 105	# s8
	.byte 122	# s8
	.byte 101	# s8
	.byte 44	# s8
	.byte 32	# s8
	.byte 77	# s8
	.byte 66	# s8
	.byte 121	# s8
	.byte 116	# s8
	.byte 101	# s8
	.byte 47	# s8
	.byte 115	# s8
	.byte 101	# s8
	.byte 99	# s8
	.byte 10	# s8
	.byte 0	# s8
	.type	__STRING.0,@object
	.size	__STRING.0,23
	.data
	.text
# -- Begin  main
# mark_begin;
       .align    4,0x90
	.globl   main
main:
.B3.1:                          # Preds .B3.0
        pushl     %ebx                                          #48.13
        movl      %esp, %ebx                                    #48.13
        andl      $-8, %esp                                     #48.13
        pushl     %edi                                          #48.13
        pushl     %esi                                          #48.13
        pushl     %ebp                                          #48.13
        subl      $84, %esp                                     #48.13
        pushl     %eax                                          #48.13
        pushl     %eax                                          #48.13
        stmxcsr   (%esp)                                        #48.13
        popl      %eax                                          #48.13
        orl       $32768, %eax                                  #48.13
        pushl     %eax                                          #48.13
        ldmxcsr   (%esp)                                        #48.13
        popl      %eax                                          #48.13
        popl      %eax                                          #48.13
        push      $8                                            #54.8
        call      malloc                                        #54.8
                                # LOE eax esi
.B3.31:                         # Preds .B3.1
        movl      %eax, %ebp                                    #54.8
                                # LOE ebp esi
.B3.2:                          # Preds .B3.31
        push      $8                                            #55.8
        call      malloc                                        #55.8
                                # LOE eax ebp esi
.B3.32:                         # Preds .B3.2
        movl      %eax, %edi                                    #55.8
                                # LOE ebp esi edi
.B3.3:                          # Preds .B3.32
        push      $__STRING.0                                   #57.3
        call      printf                                        #57.3
                                # LOE ebp esi edi
.B3.33:                         # Preds .B3.3
        addl      $12, %esp                                     #57.3
                                # LOE ebp esi edi
.B3.4:                          # Preds .B3.33
        movl      $1, 16(%esp)                                  #58.8
        movl      %edi, 52(%esp)                                #58.8
        jmp       .B3.5         # Prob 100%                     #58.8
                                # LOE ebp
.B3.27:                         # Preds .B3.26
        movl      %edx, 16(%esp)                                #58.8
                                # LOE ebp
.B3.5:                          # Preds .B3.4 .B3.27
        pushl     %ebp                                          #63.7
        call      free                                          #63.7
                                # LOE
.B3.6:                          # Preds .B3.5
        movl      56(%esp), %eax                                #64.7
        pushl     %eax                                          #64.7
        call      free                                          #64.7
                                # LOE
.B3.7:                          # Preds .B3.6
        movl      24(%esp), %edx                                #65.30
        lea       2(%edx,%edx), %eax                            #65.30
        addl      %eax, %eax                                    #65.30
        addl      %eax, %eax                                    #65.30
        pushl     %eax                                          #65.30
        call      malloc                                        #65.30
                                # LOE eax
.B3.34:                         # Preds .B3.7
        movl      %eax, %ebp                                    #65.30
                                # LOE ebp
.B3.8:                          # Preds .B3.34
        movl      28(%esp), %ecx                                #66.30
        lea       2(%ecx,%ecx), %edx                            #66.30
        addl      %edx, %edx                                    #66.30
        addl      %edx, %edx                                    #66.30
        pushl     %edx                                          #66.30
        call      malloc                                        #66.30
                                # LOE eax ebp
.B3.35:                         # Preds .B3.8
        movl      %eax, 68(%esp)                                #66.30
        addl      $16, %esp                                     #66.30
                                # LOE eax ebp al ah
.B3.9:                          # Preds .B3.35
        movl      %eax, %edx                                    #75.4
        lea       8(%ebp), %edi                                 #71.4
        movl      %ebp, %esi                                    #69.12
        andl      $15, %esi                                     #69.12
        cmove     %ebp, %edi                                    #71.4
        movl      %edx, %ecx                                    #73.12
        andl      $15, %ecx                                     #73.12
        lea       8(%edx), %esi                                 #75.4
        cmove     %edx, %esi                                    #75.4
        movl      16(%esp), %edx                                #78.14
        testl     %edx, %edx                                    #78.2
        movl      $0, %ecx                                      #78.6
        jle       .B3.13        # Prob 2%                       #78.2
        .align    4,0x90
                                # LOE edx ecx ebp esi edi
.B3.11:                         # Preds .B3.9 .B3.11
        cvtsi2sd  %ecx, %xmm0                                   #79.22
        mulsd     _2il0floatpacket.1, %xmm0                     #79.22
        movsd     %xmm0, (%edi,%ecx,8)                          #79.4
        addl      $1, %ecx                                      #78.20
        cmpl      %edx, %ecx                                    #78.2
        jl        .B3.11        # Prob 97%                      #78.2
                                # LOE edx ecx ebp esi edi
.B3.13:                         # Preds .B3.11 .B3.9
        call      get_rt                                        #82.7
                                # LOE eax ebp esi edi
.B3.37:                         # Preds .B3.13
        movl      %edx, 44(%esp)                                #82.7
        movl      %eax, 40(%esp)                                #82.7
                                # LOE ebp esi edi
.B3.14:                         # Preds .B3.37
        lea       16(%esp), %edx                                #83.8
        pushl     %esi                                          #83.19
        pushl     %edi                                          #83.19
        pushl     %edx                                          #83.19
        call      copy_                                         #83.19
                                # LOE ebp esi edi
.B3.38:                         # Preds .B3.14
        addl      $12, %esp                                     #83.19
                                # LOE ebp esi edi
.B3.15:                         # Preds .B3.38
        call      get_rt                                        #84.7
                                # LOE eax ebp esi edi
.B3.16:                         # Preds .B3.15
        movl      40(%esp), %ecx                                #85.11
        fldl      _2il0floatpacket.3                            #87.13
        subl      %ecx, %eax                                    #85.11
        movl      44(%esp), %ecx                                #85.11
        movl      %eax, 24(%esp)                                #87.13
        sbbl      %ecx, %edx                                    #85.11
        movl      16(%esp), %ecx                                #86.7
        movl      %edx, 28(%esp)                                #87.13
        fildll    24(%esp)                                      #87.13
        fstpl     72(%esp)                                      #87.13
        fldl      72(%esp)                                      #87.13
        addl      %ecx, %ecx                                    #86.7
        addl      %ecx, %ecx                                    #86.7
        addl      %ecx, %ecx                                    #86.7
        addl      %ecx, %ecx                                    #86.7
        fcomi     %st(1), %st                                   #87.2
        movl      %ecx, 40(%esp)                                #86.7
        movl      %ecx, 56(%esp)                                #86.7
        movl      $0, %ecx                                      #86.7
        movl      %ecx, 64(%esp)                                #86.7
        jb        .B3.17        # Prob 22%                      #87.2
                                # LOE ebp esi edi f1 f2
.B3.48:                         # Preds .B3.16
        fstp      %st(1)                                        #
        jmp       .B3.25        # Prob 100%                     #
                                # LOE ebp f1
.B3.17:                         # Preds .B3.16
        fdivrp    %st, %st(1)                                   #88.20
        fstpl     32(%esp)                                      #88.20
        cvttsd2si 32(%esp), %edx                                #88.20
        movl      %edx, 68(%esp)                                #88.20
        call      get_rt                                        #89.9
                                # LOE eax ebp esi edi
.B3.42:                         # Preds .B3.17
        movl      %edx, 60(%esp)                                #89.9
        movl      %eax, 40(%esp)                                #89.9
                                # LOE ebp esi edi
.B3.18:                         # Preds .B3.42
        movl      68(%esp), %edx                                #90.4
        xorl      %eax, %eax                                    #90.8
        testl     %edx, %edx                                    #90.4
        jle       .B3.23        # Prob 2%                       #90.4
                                # LOE eax ebp esi edi
.B3.19:                         # Preds .B3.18
        movl      %ebp, 48(%esp)                                #
        movl      %eax, %ebp                                    #
        .align    4,0x90
                                # LOE ebp esi edi
.B3.20:                         # Preds .B3.19 .B3.21
        lea       16(%esp), %edx                                #91.9
        pushl     %esi                                          #91.20
        pushl     %edi                                          #91.20
        pushl     %edx                                          #91.20
        call      copy_                                         #91.20
                                # LOE ebp esi edi
.B3.43:                         # Preds .B3.20
        addl      $12, %esp                                     #91.20
                                # LOE ebp esi edi
.B3.21:                         # Preds .B3.43
        movl      68(%esp), %edx                                #90.4
        addl      $1, %ebp                                      #90.20
        cmpl      %edx, %ebp                                    #90.4
        jl        .B3.20        # Prob 90%                      #90.4
                                # LOE ebp esi edi
.B3.22:                         # Preds .B3.21
        movl      48(%esp), %ebp                                #
                                # LOE ebp
.B3.23:                         # Preds .B3.22 .B3.18
        call      get_rt                                        #93.9
                                # LOE eax ebp
.B3.45:                         # Preds .B3.23
        movl      %eax, 44(%esp)                                #93.9
        movl      %edx, %esi                                    #93.9
                                # LOE eax ebp esi al ah
.B3.24:                         # Preds .B3.45
        movl      40(%esp), %edi                                #94.13
        movl      %eax, %ecx                                    #94.13
        movl      68(%esp), %eax                                #95.10
        cltd                                                    #95.10
        subl      %edi, %ecx                                    #94.13
        movl      %ecx, 44(%esp)                                #94.13
        movl      60(%esp), %ecx                                #94.13
        pushl     %edx                                          #95.4
        sbbl      %ecx, %esi                                    #94.13
        pushl     %eax                                          #95.4
        movl      72(%esp), %edi                                #95.4
        pushl     %edi                                          #95.4
        movl      68(%esp), %ecx                                #95.4
        pushl     %ecx                                          #95.4
        call      __allmul                                      #95.4
                                # LOE eax ebp esi
.B3.47:                         # Preds .B3.24
        movl      16(%esp), %edi                                #100.21
        movl      44(%esp), %ecx                                #99.24
        movl      %edx, 64(%esp)                                #95.4
        addl      %edi, %edi                                    #100.21
        movl      %eax, 56(%esp)                                #95.4
        addl      %edi, %edi                                    #100.21
        movl      %ecx, 24(%esp)                                #99.24
        movl      %esi, 28(%esp)                                #99.24
        fildll    24(%esp)                                      #99.24
        fstpl     72(%esp)                                      #99.24
        fldl      72(%esp)                                      #99.24
        addl      %edi, %edi                                    #100.21
        addl      %edi, %edi                                    #100.21
        movl      %edi, 40(%esp)                                #100.21
                                # LOE ebp f1
.B3.25:                         # Preds .B3.47 .B3.48
        movl      56(%esp), %esi                                #99.11
        movl      64(%esp), %ecx                                #99.11
        movl      40(%esp), %edx                                #100.42
        movl      %esi, 24(%esp)                                #99.11
        movl      %ecx, 28(%esp)                                #99.11
        fildll    24(%esp)                                      #99.11
        fmull     _2il0floatpacket.7                            #99.11
        fdivp     %st, %st(1)                                   #99.24
        movl      $__STRING.1, (%esp)                           #100.42
        fstpl     8(%esp)                                       #100.42
        movl      %edx, 4(%esp)                                 #100.42
        call      printf                                        #100.42
                                # LOE ebp
.B3.26:                         # Preds .B3.25
        movl      16(%esp), %edx                                #58.28
        addl      %edx, %edx                                    #58.28
        cmpl      $104857600, %edx                              #58.3
        jl        .B3.27        # Prob 90%                      #58.3
                                # LOE edx ebp
.B3.28:                         # Preds .B3.26                  # Infreq
        xorl      %eax, %eax                                    #102.1
        addl      $84, %esp                                     #102.1
        popl      %ebp                                          #102.1
        popl      %esi                                          #102.1
        popl      %edi                                          #102.1
        movl      %ebx, %esp                                    #102.1
        popl      %ebx                                          #102.1
        ret                                                     #102.1
        .align    4,0x90
                                # LOE
# mark_end;
	.type	main,@function
	.size	main,.-main
	.data
# -- End  main
	.data
# End

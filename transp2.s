# -- Machine type PW
# mark_description "Intel(R) C++ Compiler for 32-bit applications, Version 6.0   Build 020722Z";
# mark_description "-long_double -Zp16 -S -xW";
	.ident "Intel(R) C++ Compiler for 32-bit applications, Version 6.0   Build 020722Z"
	.ident "-long_double -Zp16 -S -xW"
	.file "transp_3.c"
	.text
	.data
	.align 4
	.bss
	.align 4
	.ident "-?comment:Intel(R) C++ Compiler for 32-bit applications, Version 6.0   Build 020722Z  : transp_3.c : -long_double -Zp16 -S -xW"
	.data
	.text
# -- Begin  transp_
# mark_begin;
       .align    4,0x90
# parameter 1: 44 + %esp
# parameter 2: 48 + %esp
# parameter 3: 52 + %esp
# parameter 4: 56 + %esp
	.globl   transp_
transp_:
.B1.1:                          # Preds .B1.0
        pushl     %edi                                          #6.58
        pushl     %esi                                          #6.58
        pushl     %ebp                                          #6.58
        pushl     %ebx                                          #6.58
        subl      $24, %esp                                     #6.58
        movl      44(%esp), %esi                                #6.6
        xorl      %edi, %edi                                    #10.8
        movl      48(%esp), %eax                                #6.6
        movl      52(%esp), %edx                                #6.6
        movl      (%eax), %eax                                  #10.17
        addl      $-32, %eax                                    #10.17
        testl     %eax, %eax                                    #10.3
        jl        .B1.13        # Prob 2%                       #10.3
                                # LOE eax edx esi edi
.B1.2:                          # Preds .B1.1
        movl      (%esi), %ebp                                  #12.16
        lea       -32(%ebp), %ecx                               #12.16
        lea       (%edi,%edi), %ebx                             #12.16
        addl      %ebx, %ebx                                    #12.16
        addl      %ebx, %ebx                                    #12.16
        addl      %ebx, %ebx                                    #12.16
                                # LOE eax edx ecx ebx ebp esi edi
.B1.3:                          # Preds .B1.2 .B1.11
        movl      56(%esp), %esi                                #11.5
        lea       (%esi,%ebx), %esi                             #11.5
        movl      %esi, 4(%esp)                                 #11.5
        xorl      %esi, %esi                                    #12.7
        testl     %ecx, %ecx                                    #12.2
        jl        .B1.11        # Prob 2%                       #12.2
                                # LOE eax edx ecx ebx ebp esi edi
.B1.4:                          # Preds .B1.3
        movl      %ebx, 12(%esp)                                #
        movl      44(%esp), %ebx                                #
        movl      %edi, 8(%esp)                                 #
        lea       (%esi,%esi), %eax                             #
        movl      4(%esp), %edi                                 #
        addl      %eax, %eax                                    #
        addl      %eax, %eax                                    #
        addl      %eax, %eax                                    #
                                # LOE eax edx ebx esi edi
.B1.5:                          # Preds .B1.4 .B1.9
        movl      %edi, 4(%esp)                                 #14.9
        movl      %edx, %ebp                                    #11.2
        xorl      %ecx, %ecx                                    #14.9
        .align    4,0x90
                                # LOE eax edx ecx ebx ebp esi
.B1.6:                          # Preds .B1.6 .B1.5
        movapd     (%ebp,%eax), %xmm0                            #16.17
        movapd     16(%ebp,%eax), %xmm1                          #16.17
        movapd     32(%ebp,%eax), %xmm2                          #16.17
        movapd     48(%ebp,%eax), %xmm3                          #16.17
        movapd     64(%ebp,%eax), %xmm4                          #16.17
        movapd     80(%ebp,%eax), %xmm5                          #16.17
        movapd     96(%ebp,%eax), %xmm6                          #16.17
        movapd     112(%ebp,%eax), %xmm7                         #16.17
        movapd     %xmm0, buf(%ecx)                              #16.9
        movapd     %xmm1, buf+16(%ecx)                           #16.9
        movapd     %xmm2, buf+32(%ecx)                           #16.9
        movapd     %xmm3, buf+48(%ecx)                           #16.9
        movapd     %xmm4, buf+64(%ecx)                           #16.9
        movapd     %xmm5, buf+80(%ecx)                           #16.9
        movapd     %xmm6, buf+96(%ecx)                           #16.9
        movapd     %xmm7, buf+112(%ecx)                          #16.9
        movapd     128(%ebp,%eax), %xmm0                         #16.17
        movapd     144(%ebp,%eax), %xmm1                         #16.17
        movapd     160(%ebp,%eax), %xmm2                         #16.17
        movapd     176(%ebp,%eax), %xmm3                         #16.17
        movapd     192(%ebp,%eax), %xmm4                         #16.17
        movapd     208(%ebp,%eax), %xmm5                         #16.17
        movapd     224(%ebp,%eax), %xmm6                         #16.17
        movapd     240(%ebp,%eax), %xmm7                         #16.17
        movapd     %xmm0, buf+128(%ecx)                          #16.9
        movapd     %xmm1, buf+144(%ecx)                          #16.9
        movapd     %xmm2, buf+160(%ecx)                          #16.9
        movapd     %xmm3, buf+176(%ecx)                          #16.9
        movapd     %xmm4, buf+192(%ecx)                          #16.9
        movapd     %xmm5, buf+208(%ecx)                          #16.9
        movapd     %xmm6, buf+224(%ecx)                          #16.9
        movapd     %xmm7, buf+240(%ecx)                          #16.9
        movapd     256(%ebp,%eax), %xmm0                         #16.17
        movapd     272(%ebp,%eax), %xmm1                         #16.17
        movapd     288(%ebp,%eax), %xmm2                         #16.17
        movapd     304(%ebp,%eax), %xmm3                         #16.17
        movapd     320(%ebp,%eax), %xmm4                         #16.17
        movapd     336(%ebp,%eax), %xmm5                         #16.17
        movapd     352(%ebp,%eax), %xmm6                         #16.17
        movapd     368(%ebp,%eax), %xmm7                         #16.17
        movapd     %xmm0, buf+256(%ecx)                          #16.9
        movapd     %xmm1, buf+272(%ecx)                          #16.9
        movapd     %xmm2, buf+288(%ecx)                          #16.9
        movapd     %xmm3, buf+304(%ecx)                          #16.9
        movapd     %xmm4, buf+320(%ecx)                          #16.9
        movapd     %xmm5, buf+336(%ecx)                          #16.9
        movapd     %xmm6, buf+352(%ecx)                          #16.9
        movapd     %xmm7, buf+368(%ecx)                          #16.9
        movapd     384(%ebp,%eax), %xmm0                         #16.17
        movapd     400(%ebp,%eax), %xmm1                         #16.17
        movapd     416(%ebp,%eax), %xmm2                         #16.17
        movapd     432(%ebp,%eax), %xmm3                         #16.17
        movapd     448(%ebp,%eax), %xmm4                         #16.17
        movapd     464(%ebp,%eax), %xmm5                         #16.17
        movapd     480(%ebp,%eax), %xmm6                         #16.17
        movapd     496(%ebp,%eax), %xmm7                         #16.17
        movapd     %xmm0, buf+384(%ecx)                          #16.9
        movapd     %xmm1, buf+400(%ecx)                          #16.9
        movapd     %xmm2, buf+416(%ecx)                          #16.9
        movapd     %xmm3, buf+432(%ecx)                          #16.9
        movapd     %xmm4, buf+448(%ecx)                          #16.9
        movapd     %xmm5, buf+464(%ecx)                          #16.9
        movapd     %xmm6, buf+480(%ecx)                          #16.9
        movapd     %xmm7, buf+496(%ecx)                          #16.9
        addl      $512, %ecx                                    #14.23
        movl      (%ebx), %edi                                  #18.7
        addl      %edi, %edi                                    #18.3
        addl      %edi, %edi                                    #18.3
        addl      %edi, %edi                                    #18.3
        addl      %edi, %edi                                    #18.3
        addl      %edi, %ebp                                    #18.3
        cmpl      $16384, %ecx                                  #14.4
        jl        .B1.6         # Prob 96%                      #14.4
                                # LOE eax edx ecx ebx ebp esi
.B1.7:                          # Preds .B1.6
        movl      4(%esp), %edi                                 #
        movl      48(%esp), %ebx                                #20.9
        movl      %esi, 16(%esp)                                #20.9
        xorl      %ecx, %ecx                                    #20.9
        .align    4,0x90
                                # LOE eax edx ecx ebx edi
.B1.8:                          # Preds .B1.8 .B1.7
        movapd     buf(%ecx), %xmm0                              #22.16
        movapd     buf+512(%ecx), %xmm1                          #22.16
        movapd     buf+1024(%ecx), %xmm2                         #22.16
        movapd     buf+1536(%ecx), %xmm3                         #22.16
        movapd     buf+2048(%ecx), %xmm4                         #22.16
        movapd     buf+2560(%ecx), %xmm5                         #22.16
        movapd     buf+3072(%ecx), %xmm6                         #22.16
        movapd     buf+3584(%ecx), %xmm7                         #22.16
        movntpd     %xmm0, (%edi)                                 #22.5
        movntpd     %xmm1, 16(%edi)                               #22.5
        movntpd     %xmm2, 32(%edi)                               #22.5
        movntpd     %xmm3, 48(%edi)                               #22.5
        movntpd     %xmm4, 64(%edi)                               #22.5
        movntpd     %xmm5, 80(%edi)                               #22.5
        movntpd     %xmm6, 96(%edi)                               #22.5
        movntpd     %xmm7, 112(%edi)                              #22.5
        movapd     buf+4096(%ecx), %xmm0                         #22.16
        movapd     buf+4608(%ecx), %xmm1                         #22.16
        movapd     buf+5120(%ecx), %xmm2                         #22.16
        movapd     buf+5632(%ecx), %xmm3                         #22.16
        movapd     buf+6144(%ecx), %xmm4                         #22.16
        movapd     buf+6656(%ecx), %xmm5                         #22.16
        movapd     buf+7168(%ecx), %xmm6                         #22.16
        movapd     buf+7680(%ecx), %xmm7                         #22.16
        movntpd     %xmm0, 128(%edi)                              #22.5
        movntpd     %xmm1, 144(%edi)                              #22.5
        movntpd     %xmm2, 160(%edi)                              #22.5
        movntpd     %xmm3, 176(%edi)                              #22.5
        movntpd     %xmm4, 192(%edi)                              #22.5
        movntpd     %xmm5, 208(%edi)                              #22.5
        movntpd     %xmm6, 224(%edi)                              #22.5
        movntpd     %xmm7, 240(%edi)                              #22.5
        movapd     buf+8192(%ecx), %xmm0                         #22.16
        movapd     buf+8704(%ecx), %xmm1                         #22.16
        movapd     buf+9216(%ecx), %xmm2                         #22.16
        movapd     buf+9728(%ecx), %xmm3                         #22.16
        movapd     buf+10240(%ecx), %xmm4                        #22.16
        movapd     buf+10752(%ecx), %xmm5                        #22.16
        movapd     buf+11264(%ecx), %xmm6                        #22.16
        movapd     buf+11776(%ecx), %xmm7                        #22.16
        movntpd     %xmm0, 256(%edi)                              #22.5
        movntpd     %xmm1, 272(%edi)                              #22.5
        movntpd     %xmm2, 288(%edi)                              #22.5
        movntpd     %xmm3, 304(%edi)                              #22.5
        movntpd     %xmm4, 320(%edi)                              #22.5
        movntpd     %xmm5, 336(%edi)                              #22.5
        movntpd     %xmm6, 352(%edi)                              #22.5
        movntpd     %xmm7, 368(%edi)                              #22.5
        movapd     buf+12288(%ecx), %xmm0                        #22.16
        movapd     buf+12800(%ecx), %xmm1                        #22.16
        movapd     buf+13312(%ecx), %xmm2                        #22.16
        movapd     buf+13824(%ecx), %xmm3                        #22.16
        movapd     buf+14336(%ecx), %xmm4                        #22.16
        movapd     buf+14848(%ecx), %xmm5                        #22.16
        movapd     buf+15360(%ecx), %xmm6                        #22.16
        movapd     buf+15872(%ecx), %xmm7                        #22.16
        movntpd     %xmm0, 384(%edi)                              #22.5
        movntpd     %xmm1, 400(%edi)                              #22.5
        movntpd     %xmm2, 416(%edi)                              #22.5
        movntpd     %xmm3, 432(%edi)                              #22.5
        movntpd     %xmm4, 448(%edi)                              #22.5
        movntpd     %xmm5, 464(%edi)                              #22.5
        movntpd     %xmm6, 480(%edi)                              #22.5
        movntpd     %xmm7, 496(%edi)                              #22.5
        addl      $16, %ecx                                     #20.23
        movl      (%ebx), %ebp                                  #24.8
        lea       (%ebp,%ebp), %esi                             #24.3
        addl      %esi, %esi                                    #24.3
        addl      %esi, %esi                                    #24.3
        addl      %esi, %esi                                    #24.3
        addl      %esi, %edi                                    #24.3
        cmpl      $512, %ecx                                    #20.4
        jl        .B1.8         # Prob 96%                      #20.4
                                # LOE eax edx ecx ebx ebp edi
.B1.9:                          # Preds .B1.8
        movl      16(%esp), %esi                                #
        movl      44(%esp), %ebx                                #
        movl      %ebp, 20(%esp)                                #
        movl      (%ebx), %ebp                                  #12.16
        addl      $512, %eax                                    #12.23
        addl      $32, %esi                                     #12.23
        lea       -32(%ebp), %ecx                               #12.16
        cmpl      %ecx, %esi                                    #12.2
        jle       .B1.5         # Prob 90%                      #12.2
                                # LOE eax edx ecx ebx ebp esi edi
.B1.10:                         # Preds .B1.9
        movl      12(%esp), %ebx                                #
        movl      20(%esp), %eax                                #
        movl      8(%esp), %edi                                 #
        addl      $-32, %eax                                    #10.17
                                # LOE eax edx ecx ebx ebp edi
.B1.11:                         # Preds .B1.10 .B1.3
        movl      %ebp, %esi                                    #27.2
        shll      $9, %esi                                      #27.2
        addl      %esi, %edx                                    #27.2
        addl      $512, %ebx                                    #10.24
        addl      $32, %edi                                     #10.24
        cmpl      %eax, %edi                                    #10.3
        jle       .B1.3         # Prob 90%                      #10.3
                                # LOE eax edx ecx ebx ebp edi
.B1.13:                         # Preds .B1.11 .B1.1
        addl      $24, %esp                                     #29.1
        popl      %ebx                                          #29.1
        popl      %ebp                                          #29.1
        popl      %esi                                          #29.1
        popl      %edi                                          #29.1
        ret                                                     #29.1
        .align    4,0x90
                                # LOE
# mark_end;
	.type	transp_,@function
	.size	transp_,.-transp_
	.bss
buf:
	.type buf,@object
	.align 16,0x00
	.size buf,16384
	.space 16384		
	.data
# -- End  transp_
	.data
# End

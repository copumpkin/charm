logger:
(__TEXT,__text) section
start:
00002000	e59d0000	ldr	r0, [sp]
00002004	e28d1004	add	r1, sp, #4	; 0x4
00002008	e2804001	add	r4, r0, #1	; 0x1
0000200c	e0812104	add	r2, r1, r4, lsl #2
00002010	e3cdd007	bic	sp, sp, #7	; 0x7
00002014	e1a03002	mov	r3, r2
00002018	e4934004	ldr	r4, [r3], #4
0000201c	e3540000	cmp	r4, #0	; 0x0
00002020	1afffffc	bne	0x2018
00002024	e59fc018	ldr	ip, [pc, #24]	; 0x2044
00002028	e08fc00c	add	ip, pc, ip
0000202c	e59cc000	ldr	ip, [ip]
00002030	e12fff3c	blx	ip
00002034	e59fc00c	ldr	ip, [pc, #12]	; 0x2048
00002038	e08fc00c	add	ip, pc, ip
0000203c	e59cc000	ldr	ip, [ip]
00002040	e12fff1c	bx	ip
00002044	00009fd0	ldreqd	r9, [r0], -r0
00002048	00009fc4	andeq	r9, r0, r4, asr #31
dyld_stub_binding_helper:
0000204c	e52dc004	str	ip, [sp, #-4]!
00002050	e59fc00c	ldr	ip, [pc, #12]	; 0x2064
00002054	e79fc00c	ldr	ip, [pc, ip]
00002058	e52dc004	str	ip, [sp, #-4]!
0000205c	e59fc004	ldr	ip, [pc, #4]	; 0x2068
00002060	e79ff00c	ldr	pc, [pc, ip]
00002064	0000a05c	andeq	r10, r0, ip, asr r0
00002068	0000a0bc	streqh	r10, [r0], -ip
__dyld_func_lookup:
0000206c	e59fc000	ldr	ip, [pc, #0]	; 0x2074
00002070	e79ff00c	ldr	pc, [pc, ip]
00002074	0000a0b0	streqh	r10, [r0], -r0
_cxa_atexit_check_2:
00002078	e92d4080	stmdb	sp!, {r7, lr}
0000207c	e28d7000	add	r7, sp, #0	; 0x0
00002080	e24dd004	sub	sp, sp, #4	; 0x4
00002084	e58d0000	str	r0, [sp]
00002088	e59d2000	ldr	r2, [sp]
0000208c	e3a03001	mov	r3, #1	; 0x1
00002090	e5823000	str	r3, [r2]
00002094	e247d000	sub	sp, r7, #0	; 0x0
00002098	e8bd8080	ldmia	sp!, {r7, pc}
_cxa_atexit_check_1:
0000209c	e92d4080	stmdb	sp!, {r7, lr}
000020a0	e28d7000	add	r7, sp, #0	; 0x0
000020a4	e24dd008	sub	sp, sp, #8	; 0x8
000020a8	e58d0000	str	r0, [sp]
000020ac	e59d3000	ldr	r3, [sp]
000020b0	e58d3004	str	r3, [sp, #4]
000020b4	e59d3004	ldr	r3, [sp, #4]
000020b8	e593c004	ldr	ip, [r3, #4]
000020bc	e59f3030	ldr	r3, [pc, #48]	; 0x20f4
000020c0	e08f3003	add	r3, pc, r3
000020c4	e1a00003	mov	r0, r3
000020c8	e59d1000	ldr	r1, [sp]
000020cc	e59d2000	ldr	r2, [sp]
000020d0	e12fff3c	blx	ip
000020d4	e1a03000	mov	r3, r0
000020d8	e3530000	cmp	r3, #0	; 0x0
000020dc	0a000002	beq	0x20ec
000020e0	e59d2004	ldr	r2, [sp, #4]
000020e4	e3e03000	mvn	r3, #0	; 0x0
000020e8	e5823000	str	r3, [r2]
000020ec	e247d000	sub	sp, r7, #0	; 0x0
000020f0	e8bd8080	ldmia	sp!, {r7, pc}
000020f4	ffffffb0	undefined instruction 0xffffffb0
_check_cxa_atexit:
000020f8	e92d4090	stmdb	sp!, {r4, r7, lr}
000020fc	e28d7004	add	r7, sp, #4	; 0x4
00002100	e24dd014	sub	sp, sp, #20	; 0x14
00002104	e58d0008	str	r0, [sp, #8]
00002108	e58d1004	str	r1, [sp, #4]
0000210c	e3a03000	mov	r3, #0	; 0x0
00002110	e58d300c	str	r3, [sp, #12]
00002114	e59d3008	ldr	r3, [sp, #8]
00002118	e58d3010	str	r3, [sp, #16]
0000211c	e28d200c	add	r2, sp, #12	; 0xc
00002120	e28dc00c	add	ip, sp, #12	; 0xc
00002124	e59d4008	ldr	r4, [sp, #8]
00002128	e59f3074	ldr	r3, [pc, #116]	; 0x21a4
0000212c	e08f3003	add	r3, pc, r3
00002130	e1a00003	mov	r0, r3
00002134	e1a01002	mov	r1, r2
00002138	e1a0200c	mov	r2, ip
0000213c	e12fff34	blx	r4
00002140	e1a03000	mov	r3, r0
00002144	e3530000	cmp	r3, #0	; 0x0
00002148	0a000002	beq	0x2158
0000214c	e3e03000	mvn	r3, #0	; 0x0
00002150	e58d3000	str	r3, [sp]
00002154	ea00000e	b	0x2194
00002158	e28d300c	add	r3, sp, #12	; 0xc
0000215c	e59d2004	ldr	r2, [sp, #4]
00002160	e1a00003	mov	r0, r3
00002164	e12fff32	blx	r2
00002168	e59d300c	ldr	r3, [sp, #12]
0000216c	e3530000	cmp	r3, #0	; 0x0
00002170	1a000005	bne	0x218c
00002174	e28d300c	add	r3, sp, #12	; 0xc
00002178	e59d2004	ldr	r2, [sp, #4]
0000217c	e1a00003	mov	r0, r3
00002180	e12fff32	blx	r2
00002184	e3a03000	mov	r3, #0	; 0x0
00002188	e58d300c	str	r3, [sp, #12]
0000218c	e59d300c	ldr	r3, [sp, #12]
00002190	e58d3000	str	r3, [sp]
00002194	e59d3000	ldr	r3, [sp]
00002198	e1a00003	mov	r0, r3
0000219c	e247d004	sub	sp, r7, #4	; 0x4
000021a0	e8bd8090	ldmia	sp!, {r4, r7, pc}
000021a4	ffffff68	undefined instruction 0xffffff68
_get_globals:
000021a8	e92d4080	stmdb	sp!, {r7, lr}
000021ac	e28d7000	add	r7, sp, #0	; 0x0
000021b0	e24dd014	sub	sp, sp, #20	; 0x14
000021b4	e28d3004	add	r3, sp, #4	; 0x4
000021b8	e3a0000e	mov	r0, #14	; 0xe
000021bc	e1a01003	mov	r1, r3
000021c0	eb00246c	bl	0xb378	; symbol stub for: __keymgr_get_and_lock_processwide_ptr_2
000021c4	e1a03000	mov	r3, r0
000021c8	e3530000	cmp	r3, #0	; 0x0
000021cc	0a000002	beq	0x21dc
000021d0	e3a03000	mov	r3, #0	; 0x0
000021d4	e58d3000	str	r3, [sp]
000021d8	ea000062	b	0x2368
000021dc	e59d3004	ldr	r3, [sp, #4]
000021e0	e58d3008	str	r3, [sp, #8]
000021e4	e59d3008	ldr	r3, [sp, #8]
000021e8	e3530000	cmp	r3, #0	; 0x0
000021ec	1a00000a	bne	0x221c
000021f0	e3a00014	mov	r0, #20	; 0x14
000021f4	e3a01001	mov	r1, #1	; 0x1
000021f8	eb002464	bl	0xb390	; symbol stub for: _calloc
000021fc	e1a03000	mov	r3, r0
00002200	e58d3008	str	r3, [sp, #8]
00002204	e59d3008	ldr	r3, [sp, #8]
00002208	e3530000	cmp	r3, #0	; 0x0
0000220c	1a000002	bne	0x221c
00002210	e3a03000	mov	r3, #0	; 0x0
00002214	e58d3000	str	r3, [sp]
00002218	ea000052	b	0x2368
0000221c	e59d3008	ldr	r3, [sp, #8]
00002220	e5d33003	ldrb	r3, [r3, #3]
00002224	e3530000	cmp	r3, #0	; 0x0
00002228	1a000046	bne	0x2348
0000222c	e59f3144	ldr	r3, [pc, #324]	; 0x2378
00002230	e08f3003	add	r3, pc, r3
00002234	e1a00003	mov	r0, r3
00002238	e3a01010	mov	r1, #16	; 0x10
0000223c	eb002456	bl	0xb39c	; symbol stub for: _dlopen
00002240	e1a03000	mov	r3, r0
00002244	e58d300c	str	r3, [sp, #12]
00002248	e59d300c	ldr	r3, [sp, #12]
0000224c	e3530000	cmp	r3, #0	; 0x0
00002250	0a00003f	beq	0x2354
00002254	e59d000c	ldr	r0, [sp, #12]
00002258	e59f311c	ldr	r3, [pc, #284]	; 0x237c
0000225c	e08f3003	add	r3, pc, r3
00002260	e1a01003	mov	r1, r3
00002264	eb00244f	bl	0xb3a8	; symbol stub for: _dlsym
00002268	e1a03000	mov	r3, r0
0000226c	e1a02003	mov	r2, r3
00002270	e59d3008	ldr	r3, [sp, #8]
00002274	e5832008	str	r2, [r3, #8]
00002278	e59d000c	ldr	r0, [sp, #12]
0000227c	e59f30fc	ldr	r3, [pc, #252]	; 0x2380
00002280	e08f3003	add	r3, pc, r3
00002284	e1a01003	mov	r1, r3
00002288	eb002446	bl	0xb3a8	; symbol stub for: _dlsym
0000228c	e1a03000	mov	r3, r0
00002290	e1a02003	mov	r2, r3
00002294	e59d3008	ldr	r3, [sp, #8]
00002298	e583200c	str	r2, [r3, #12]
0000229c	e59d3008	ldr	r3, [sp, #8]
000022a0	e5933008	ldr	r3, [r3, #8]
000022a4	e3530000	cmp	r3, #0	; 0x0
000022a8	0a000029	beq	0x2354
000022ac	e59d3008	ldr	r3, [sp, #8]
000022b0	e593300c	ldr	r3, [r3, #12]
000022b4	e3530000	cmp	r3, #0	; 0x0
000022b8	0a000025	beq	0x2354
000022bc	e59d3008	ldr	r3, [sp, #8]
000022c0	e5932008	ldr	r2, [r3, #8]
000022c4	e59d3008	ldr	r3, [sp, #8]
000022c8	e593300c	ldr	r3, [r3, #12]
000022cc	e1a00002	mov	r0, r2
000022d0	e1a01003	mov	r1, r3
000022d4	ebffff87	bl	_check_cxa_atexit
000022d8	e1a03000	mov	r3, r0
000022dc	e58d3010	str	r3, [sp, #16]
000022e0	e59d3010	ldr	r3, [sp, #16]
000022e4	e3730001	cmn	r3, #1	; 0x1
000022e8	0a000019	beq	0x2354
000022ec	e59d3010	ldr	r3, [sp, #16]
000022f0	e3530000	cmp	r3, #0	; 0x0
000022f4	1a000003	bne	0x2308
000022f8	e59d3008	ldr	r3, [sp, #8]
000022fc	e3a02002	mov	r2, #2	; 0x2
00002300	e5c32003	strb	r2, [r3, #3]
00002304	ea00000f	b	0x2348
00002308	e59d000c	ldr	r0, [sp, #12]
0000230c	e59f3070	ldr	r3, [pc, #112]	; 0x2384
00002310	e08f3003	add	r3, pc, r3
00002314	e1a01003	mov	r1, r3
00002318	eb002422	bl	0xb3a8	; symbol stub for: _dlsym
0000231c	e1a03000	mov	r3, r0
00002320	e1a02003	mov	r2, r3
00002324	e59d3008	ldr	r3, [sp, #8]
00002328	e5832010	str	r2, [r3, #16]
0000232c	e59d3008	ldr	r3, [sp, #8]
00002330	e5933010	ldr	r3, [r3, #16]
00002334	e3530000	cmp	r3, #0	; 0x0
00002338	0a000005	beq	0x2354
0000233c	e59d3008	ldr	r3, [sp, #8]
00002340	e3a02010	mov	r2, #16	; 0x10
00002344	e5c32003	strb	r2, [r3, #3]
00002348	e59d3008	ldr	r3, [sp, #8]
0000234c	e58d3000	str	r3, [sp]
00002350	ea000004	b	0x2368
00002354	e3a0000e	mov	r0, #14	; 0xe
00002358	e59d1008	ldr	r1, [sp, #8]
0000235c	eb002408	bl	0xb384	; symbol stub for: __keymgr_set_and_unlock_processwide_ptr
00002360	e3a03000	mov	r3, #0	; 0x0
00002364	e58d3000	str	r3, [sp]
00002368	e59d3000	ldr	r3, [sp]
0000236c	e1a00003	mov	r0, r3
00002370	e247d000	sub	sp, r7, #0	; 0x0
00002374	e8bd8080	ldmia	sp!, {r7, pc}
00002378	00009248	andeq	r9, r0, r8, asr #4
0000237c	00009238	andeq	r9, r0, r8, lsr r2
00002380	00009224	andeq	r9, r0, r4, lsr #4
00002384	000091a4	andeq	r9, r0, r4, lsr #3
_add_routine:
00002388	e92d4080	stmdb	sp!, {r7, lr}
0000238c	e28d7000	add	r7, sp, #0	; 0x0
00002390	e24dd018	sub	sp, sp, #24	; 0x18
00002394	e58d000c	str	r0, [sp, #12]
00002398	e58d1008	str	r1, [sp, #8]
0000239c	e3a00010	mov	r0, #16	; 0x10
000023a0	eb00241e	bl	0xb420	; symbol stub for: _malloc
000023a4	e1a03000	mov	r3, r0
000023a8	e58d3010	str	r3, [sp, #16]
000023ac	e59d3010	ldr	r3, [sp, #16]
000023b0	e3530000	cmp	r3, #0	; 0x0
000023b4	1a000005	bne	0x23d0
000023b8	e3a0000e	mov	r0, #14	; 0xe
000023bc	e59d100c	ldr	r1, [sp, #12]
000023c0	eb0023ef	bl	0xb384	; symbol stub for: __keymgr_set_and_unlock_processwide_ptr
000023c4	e3e03000	mvn	r3, #0	; 0x0
000023c8	e58d3000	str	r3, [sp]
000023cc	ea00001b	b	0x2440
000023d0	e59d3010	ldr	r3, [sp, #16]
000023d4	e59d2008	ldr	r2, [sp, #8]
000023d8	e283c004	add	ip, r3, #4	; 0x4
000023dc	e1a03002	mov	r3, r2
000023e0	e8930007	ldmia	r3, {r0, r1, r2}
000023e4	e88c0007	stmia	ip, {r0, r1, r2}
000023e8	e59d300c	ldr	r3, [sp, #12]
000023ec	e5932004	ldr	r2, [r3, #4]
000023f0	e59d3010	ldr	r3, [sp, #16]
000023f4	e5832000	str	r2, [r3]
000023f8	e59d200c	ldr	r2, [sp, #12]
000023fc	e59d3010	ldr	r3, [sp, #16]
00002400	e5823004	str	r3, [r2, #4]
00002404	e3a0000e	mov	r0, #14	; 0xe
00002408	e59d100c	ldr	r1, [sp, #12]
0000240c	eb0023dc	bl	0xb384	; symbol stub for: __keymgr_set_and_unlock_processwide_ptr
00002410	e1a03000	mov	r3, r0
00002414	e58d3014	str	r3, [sp, #20]
00002418	e59d3014	ldr	r3, [sp, #20]
0000241c	e3530000	cmp	r3, #0	; 0x0
00002420	1a000002	bne	0x2430
00002424	e3a03000	mov	r3, #0	; 0x0
00002428	e58d3004	str	r3, [sp, #4]
0000242c	ea000001	b	0x2438
00002430	e3e03000	mvn	r3, #0	; 0x0
00002434	e58d3004	str	r3, [sp, #4]
00002438	e59d3004	ldr	r3, [sp, #4]
0000243c	e58d3000	str	r3, [sp]
00002440	e59d3000	ldr	r3, [sp]
00002444	e1a00003	mov	r0, r3
00002448	e247d000	sub	sp, r7, #0	; 0x0
0000244c	e8bd8080	ldmia	sp!, {r7, pc}
_run_routines:
00002450	e92d4080	stmdb	sp!, {r7, lr}
00002454	e28d7000	add	r7, sp, #0	; 0x0
00002458	e24dd014	sub	sp, sp, #20	; 0x14
0000245c	e58d000c	str	r0, [sp, #12]
00002460	e58d1008	str	r1, [sp, #8]
00002464	e59d300c	ldr	r3, [sp, #12]
00002468	e5933004	ldr	r3, [r3, #4]
0000246c	e58d3010	str	r3, [sp, #16]
00002470	e59d3010	ldr	r3, [sp, #16]
00002474	e3530000	cmp	r3, #0	; 0x0
00002478	0a000030	beq	0x2540
0000247c	e59d2010	ldr	r2, [sp, #16]
00002480	e59d3008	ldr	r3, [sp, #8]
00002484	e1520003	cmp	r2, r3
00002488	0a00002c	beq	0x2540
0000248c	e59d3010	ldr	r3, [sp, #16]
00002490	e5932000	ldr	r2, [r3]
00002494	e59d300c	ldr	r3, [sp, #12]
00002498	e5832004	str	r2, [r3, #4]
0000249c	e3a0000e	mov	r0, #14	; 0xe
000024a0	e59d100c	ldr	r1, [sp, #12]
000024a4	eb0023b6	bl	0xb384	; symbol stub for: __keymgr_set_and_unlock_processwide_ptr
000024a8	e59d3010	ldr	r3, [sp, #16]
000024ac	e5933008	ldr	r3, [r3, #8]
000024b0	e58d3004	str	r3, [sp, #4]
000024b4	e59d2004	ldr	r2, [sp, #4]
000024b8	e3520005	cmp	r2, #5	; 0x5
000024bc	8a000016	bhi	0x251c
000024c0	e3a03001	mov	r3, #1	; 0x1
000024c4	e59d2004	ldr	r2, [sp, #4]
000024c8	e1a03213	mov	r3, r3, lsl r2
000024cc	e58d3000	str	r3, [sp]
000024d0	e59d2000	ldr	r2, [sp]
000024d4	e2023015	and	r3, r2, #21	; 0x15
000024d8	e3530000	cmp	r3, #0	; 0x0
000024dc	1a000004	bne	0x24f4
000024e0	e59d2000	ldr	r2, [sp]
000024e4	e202302a	and	r3, r2, #42	; 0x2a
000024e8	e3530000	cmp	r3, #0	; 0x0
000024ec	1a000004	bne	0x2504
000024f0	ea000009	b	0x251c
000024f4	e59d3010	ldr	r3, [sp, #16]
000024f8	e5933004	ldr	r3, [r3, #4]
000024fc	e12fff33	blx	r3
00002500	ea000005	b	0x251c
00002504	e59d3010	ldr	r3, [sp, #16]
00002508	e5932004	ldr	r2, [r3, #4]
0000250c	e59d3010	ldr	r3, [sp, #16]
00002510	e593300c	ldr	r3, [r3, #12]
00002514	e1a00003	mov	r0, r3
00002518	e12fff32	blx	r2
0000251c	e59d0010	ldr	r0, [sp, #16]
00002520	eb0023af	bl	0xb3e4	; symbol stub for: _free
00002524	e3a0000e	mov	r0, #14	; 0xe
00002528	eb00238f	bl	0xb36c	; symbol stub for: __keymgr_get_and_lock_processwide_ptr
0000252c	e1a03000	mov	r3, r0
00002530	e58d300c	str	r3, [sp, #12]
00002534	e59d300c	ldr	r3, [sp, #12]
00002538	e3530000	cmp	r3, #0	; 0x0
0000253c	1affffc8	bne	0x2464
00002540	e59d300c	ldr	r3, [sp, #12]
00002544	e1a00003	mov	r0, r3
00002548	e247d000	sub	sp, r7, #0	; 0x0
0000254c	e8bd8080	ldmia	sp!, {r7, pc}
_cxa_atexit_wrapper:
00002550	e92d4080	stmdb	sp!, {r7, lr}
00002554	e28d7000	add	r7, sp, #0	; 0x0
00002558	e24dd014	sub	sp, sp, #20	; 0x14
0000255c	e58d0000	str	r0, [sp]
00002560	e59d3000	ldr	r3, [sp]
00002564	e58d3004	str	r3, [sp, #4]
00002568	e3a03000	mov	r3, #0	; 0x0
0000256c	e58d300c	str	r3, [sp, #12]
00002570	e3a03000	mov	r3, #0	; 0x0
00002574	e5cd3013	strb	r3, [sp, #19]
00002578	e3a0000e	mov	r0, #14	; 0xe
0000257c	eb00237a	bl	0xb36c	; symbol stub for: __keymgr_get_and_lock_processwide_ptr
00002580	e1a03000	mov	r3, r0
00002584	e58d3008	str	r3, [sp, #8]
00002588	e59d3008	ldr	r3, [sp, #8]
0000258c	e3530000	cmp	r3, #0	; 0x0
00002590	0a00000b	beq	0x25c4
00002594	e59d3008	ldr	r3, [sp, #8]
00002598	e5d33002	ldrb	r3, [r3, #2]
0000259c	e5cd3013	strb	r3, [sp, #19]
000025a0	e59d2008	ldr	r2, [sp, #8]
000025a4	e3a03001	mov	r3, #1	; 0x1
000025a8	e5c23002	strb	r3, [r2, #2]
000025ac	e59d3008	ldr	r3, [sp, #8]
000025b0	e5933004	ldr	r3, [r3, #4]
000025b4	e58d300c	str	r3, [sp, #12]
000025b8	e3a0000e	mov	r0, #14	; 0xe
000025bc	e59d1008	ldr	r1, [sp, #8]
000025c0	eb00236f	bl	0xb384	; symbol stub for: __keymgr_set_and_unlock_processwide_ptr
000025c4	e59d3004	ldr	r3, [sp, #4]
000025c8	e5933004	ldr	r3, [r3, #4]
000025cc	e3530000	cmp	r3, #0	; 0x0
000025d0	0a000006	beq	0x25f0
000025d4	e59d3004	ldr	r3, [sp, #4]
000025d8	e5932000	ldr	r2, [r3]
000025dc	e59d3004	ldr	r3, [sp, #4]
000025e0	e5933008	ldr	r3, [r3, #8]
000025e4	e1a00003	mov	r0, r3
000025e8	e12fff32	blx	r2
000025ec	ea000002	b	0x25fc
000025f0	e59d3004	ldr	r3, [sp, #4]
000025f4	e5933000	ldr	r3, [r3]
000025f8	e12fff33	blx	r3
000025fc	e59d3008	ldr	r3, [sp, #8]
00002600	e3530000	cmp	r3, #0	; 0x0
00002604	0a000003	beq	0x2618
00002608	e3a0000e	mov	r0, #14	; 0xe
0000260c	eb002356	bl	0xb36c	; symbol stub for: __keymgr_get_and_lock_processwide_ptr
00002610	e1a03000	mov	r3, r0
00002614	e58d3008	str	r3, [sp, #8]
00002618	e59d3008	ldr	r3, [sp, #8]
0000261c	e3530000	cmp	r3, #0	; 0x0
00002620	0a000004	beq	0x2638
00002624	e59d0008	ldr	r0, [sp, #8]
00002628	e59d100c	ldr	r1, [sp, #12]
0000262c	ebffff87	bl	_run_routines
00002630	e1a03000	mov	r3, r0
00002634	e58d3008	str	r3, [sp, #8]
00002638	e59d3008	ldr	r3, [sp, #8]
0000263c	e3530000	cmp	r3, #0	; 0x0
00002640	0a000005	beq	0x265c
00002644	e59d2008	ldr	r2, [sp, #8]
00002648	e5dd3013	ldrb	r3, [sp, #19]
0000264c	e5c23002	strb	r3, [r2, #2]
00002650	e3a0000e	mov	r0, #14	; 0xe
00002654	e59d1008	ldr	r1, [sp, #8]
00002658	eb002349	bl	0xb384	; symbol stub for: __keymgr_set_and_unlock_processwide_ptr
0000265c	e247d000	sub	sp, r7, #0	; 0x0
00002660	e8bd8080	ldmia	sp!, {r7, pc}
_atexit_common:
00002664	e92d4080	stmdb	sp!, {r7, lr}
00002668	e28d7000	add	r7, sp, #0	; 0x0
0000266c	e24dd028	sub	sp, sp, #40	; 0x28
00002670	e58d0008	str	r0, [sp, #8]
00002674	e58d1004	str	r1, [sp, #4]
00002678	ebfffeca	bl	_get_globals
0000267c	e1a03000	mov	r3, r0
00002680	e58d300c	str	r3, [sp, #12]
00002684	e59d300c	ldr	r3, [sp, #12]
00002688	e3530000	cmp	r3, #0	; 0x0
0000268c	1a000002	bne	0x269c
00002690	e3e03000	mvn	r3, #0	; 0x0
00002694	e58d3000	str	r3, [sp]
00002698	ea00006c	b	0x2850
0000269c	e59d300c	ldr	r3, [sp, #12]
000026a0	e5d33002	ldrb	r3, [r3, #2]
000026a4	e3530000	cmp	r3, #0	; 0x0
000026a8	1a000003	bne	0x26bc
000026ac	e59d300c	ldr	r3, [sp, #12]
000026b0	e5d33003	ldrb	r3, [r3, #3]
000026b4	e3530001	cmp	r3, #1	; 0x1
000026b8	1a000005	bne	0x26d4
000026bc	e59d000c	ldr	r0, [sp, #12]
000026c0	e59d1008	ldr	r1, [sp, #8]
000026c4	ebffff2f	bl	_add_routine
000026c8	e1a03000	mov	r3, r0
000026cc	e58d3000	str	r3, [sp]
000026d0	ea00005e	b	0x2850
000026d4	e59d300c	ldr	r3, [sp, #12]
000026d8	e5d33003	ldrb	r3, [r3, #3]
000026dc	e353000f	cmp	r3, #15	; 0xf
000026e0	9a000033	bls	0x27b4
000026e4	e59d3008	ldr	r3, [sp, #8]
000026e8	e5933004	ldr	r3, [r3, #4]
000026ec	e3530000	cmp	r3, #0	; 0x0
000026f0	0a000019	beq	0x275c
000026f4	e59d300c	ldr	r3, [sp, #12]
000026f8	e5933008	ldr	r3, [r3, #8]
000026fc	e58d3014	str	r3, [sp, #20]
00002700	e3a0000e	mov	r0, #14	; 0xe
00002704	e59d100c	ldr	r1, [sp, #12]
00002708	eb00231d	bl	0xb384	; symbol stub for: __keymgr_set_and_unlock_processwide_ptr
0000270c	e1a03000	mov	r3, r0
00002710	e58d3010	str	r3, [sp, #16]
00002714	e59d3010	ldr	r3, [sp, #16]
00002718	e3530000	cmp	r3, #0	; 0x0
0000271c	0a000002	beq	0x272c
00002720	e3e03000	mvn	r3, #0	; 0x0
00002724	e58d3000	str	r3, [sp]
00002728	ea000048	b	0x2850
0000272c	e59d3008	ldr	r3, [sp, #8]
00002730	e5932000	ldr	r2, [r3]
00002734	e59d3008	ldr	r3, [sp, #8]
00002738	e5933008	ldr	r3, [r3, #8]
0000273c	e59dc014	ldr	ip, [sp, #20]
00002740	e1a00002	mov	r0, r2
00002744	e1a01003	mov	r1, r3
00002748	e59d2004	ldr	r2, [sp, #4]
0000274c	e12fff3c	blx	ip
00002750	e1a03000	mov	r3, r0
00002754	e58d3000	str	r3, [sp]
00002758	ea00003c	b	0x2850
0000275c	e59d300c	ldr	r3, [sp, #12]
00002760	e5933010	ldr	r3, [r3, #16]
00002764	e58d3018	str	r3, [sp, #24]
00002768	e3a0000e	mov	r0, #14	; 0xe
0000276c	e59d100c	ldr	r1, [sp, #12]
00002770	eb002303	bl	0xb384	; symbol stub for: __keymgr_set_and_unlock_processwide_ptr
00002774	e1a03000	mov	r3, r0
00002778	e58d3010	str	r3, [sp, #16]
0000277c	e59d3010	ldr	r3, [sp, #16]
00002780	e3530000	cmp	r3, #0	; 0x0
00002784	0a000002	beq	0x2794
00002788	e3e03000	mvn	r3, #0	; 0x0
0000278c	e58d3000	str	r3, [sp]
00002790	ea00002e	b	0x2850
00002794	e59d3008	ldr	r3, [sp, #8]
00002798	e5933000	ldr	r3, [r3]
0000279c	e59d2018	ldr	r2, [sp, #24]
000027a0	e1a00003	mov	r0, r3
000027a4	e12fff32	blx	r2
000027a8	e1a03000	mov	r3, r0
000027ac	e58d3000	str	r3, [sp]
000027b0	ea000026	b	0x2850
000027b4	e59d300c	ldr	r3, [sp, #12]
000027b8	e5933008	ldr	r3, [r3, #8]
000027bc	e58d301c	str	r3, [sp, #28]
000027c0	e3a0000e	mov	r0, #14	; 0xe
000027c4	e59d100c	ldr	r1, [sp, #12]
000027c8	eb0022ed	bl	0xb384	; symbol stub for: __keymgr_set_and_unlock_processwide_ptr
000027cc	e1a03000	mov	r3, r0
000027d0	e58d3024	str	r3, [sp, #36]
000027d4	e59d3024	ldr	r3, [sp, #36]
000027d8	e3530000	cmp	r3, #0	; 0x0
000027dc	0a000002	beq	0x27ec
000027e0	e3e03000	mvn	r3, #0	; 0x0
000027e4	e58d3000	str	r3, [sp]
000027e8	ea000018	b	0x2850
000027ec	e3a0000c	mov	r0, #12	; 0xc
000027f0	eb00230a	bl	0xb420	; symbol stub for: _malloc
000027f4	e1a03000	mov	r3, r0
000027f8	e58d3020	str	r3, [sp, #32]
000027fc	e59d3020	ldr	r3, [sp, #32]
00002800	e3530000	cmp	r3, #0	; 0x0
00002804	1a000002	bne	0x2814
00002808	e3e03000	mvn	r3, #0	; 0x0
0000280c	e58d3000	str	r3, [sp]
00002810	ea00000e	b	0x2850
00002814	e59d3020	ldr	r3, [sp, #32]
00002818	e59d2008	ldr	r2, [sp, #8]
0000281c	e1a0c003	mov	ip, r3
00002820	e1a03002	mov	r3, r2
00002824	e8930007	ldmia	r3, {r0, r1, r2}
00002828	e88c0007	stmia	ip, {r0, r1, r2}
0000282c	e59dc01c	ldr	ip, [sp, #28]
00002830	e59f3028	ldr	r3, [pc, #40]	; 0x2860
00002834	e08f3003	add	r3, pc, r3
00002838	e1a00003	mov	r0, r3
0000283c	e59d1020	ldr	r1, [sp, #32]
00002840	e59d2004	ldr	r2, [sp, #4]
00002844	e12fff3c	blx	ip
00002848	e1a03000	mov	r3, r0
0000284c	e58d3000	str	r3, [sp]
00002850	e59d3000	ldr	r3, [sp]
00002854	e1a00003	mov	r0, r3
00002858	e247d000	sub	sp, r7, #0	; 0x0
0000285c	e8bd8080	ldmia	sp!, {r7, pc}
00002860	fffffd14	undefined instruction 0xfffffd14
___cxa_atexit:
00002864	e92d4080	stmdb	sp!, {r7, lr}
00002868	e28d7000	add	r7, sp, #0	; 0x0
0000286c	e24dd018	sub	sp, sp, #24	; 0x18
00002870	e58d0008	str	r0, [sp, #8]
00002874	e58d1004	str	r1, [sp, #4]
00002878	e58d2000	str	r2, [sp]
0000287c	e59d3008	ldr	r3, [sp, #8]
00002880	e58d300c	str	r3, [sp, #12]
00002884	e3a03001	mov	r3, #1	; 0x1
00002888	e58d3010	str	r3, [sp, #16]
0000288c	e59d3004	ldr	r3, [sp, #4]
00002890	e58d3014	str	r3, [sp, #20]
00002894	e28d300c	add	r3, sp, #12	; 0xc
00002898	e1a00003	mov	r0, r3
0000289c	e59d1000	ldr	r1, [sp]
000028a0	ebffff6f	bl	_atexit_common
000028a4	e1a03000	mov	r3, r0
000028a8	e1a00003	mov	r0, r3
000028ac	e247d000	sub	sp, r7, #0	; 0x0
000028b0	e8bd8080	ldmia	sp!, {r7, pc}
_atexit:
000028b4	e92d4080	stmdb	sp!, {r7, lr}
000028b8	e28d7000	add	r7, sp, #0	; 0x0
000028bc	e24dd010	sub	sp, sp, #16	; 0x10
000028c0	e58d0000	str	r0, [sp]
000028c4	e59d3000	ldr	r3, [sp]
000028c8	e58d3004	str	r3, [sp, #4]
000028cc	e3a03000	mov	r3, #0	; 0x0
000028d0	e58d3008	str	r3, [sp, #8]
000028d4	e28d3004	add	r3, sp, #4	; 0x4
000028d8	e1a00003	mov	r0, r3
000028dc	e59f301c	ldr	r3, [pc, #28]	; 0x2900
000028e0	e08f3003	add	r3, pc, r3
000028e4	e5933000	ldr	r3, [r3]
000028e8	e1a01003	mov	r1, r3
000028ec	ebffff5c	bl	_atexit_common
000028f0	e1a03000	mov	r3, r0
000028f4	e1a00003	mov	r0, r3
000028f8	e247d000	sub	sp, r7, #0	; 0x0
000028fc	e8bd8080	ldmia	sp!, {r7, pc}
00002900	00009720	andeq	r9, r0, r0, lsr #14
_initializeAudio:
00002904	e92d4080	stmdb	sp!, {r7, lr}
00002908	e28d7000	add	r7, sp, #0	; 0x0
0000290c	e24dd064	sub	sp, sp, #100	; 0x64
00002910	e58d000c	str	r0, [sp, #12]
00002914	e3a00008	mov	r0, #8	; 0x8
00002918	eb0022c0	bl	0xb420	; symbol stub for: _malloc
0000291c	e1a03000	mov	r3, r0
00002920	e58d3058	str	r3, [sp, #88]
00002924	e59d2058	ldr	r2, [sp, #88]
00002928	e59d300c	ldr	r3, [sp, #12]
0000292c	e5823004	str	r3, [r2, #4]
00002930	e59f33f0	ldr	r3, [pc, #1008]	; 0x2d28
00002934	e08f3003	add	r3, pc, r3
00002938	e28de044	add	lr, sp, #68	; 0x44
0000293c	e1a0c003	mov	ip, r3
00002940	e8bc000f	ldmia	ip!, {r0, r1, r2, r3}
00002944	e8ae000f	stmia	lr!, {r0, r1, r2, r3}
00002948	e59c3000	ldr	r3, [ip]
0000294c	e58e3000	str	r3, [lr]
00002950	e28d3044	add	r3, sp, #68	; 0x44
00002954	e3a00000	mov	r0, #0	; 0x0
00002958	e1a01003	mov	r1, r3
0000295c	eb0021fe	bl	0xb15c	; symbol stub for: _AudioComponentFindNext
00002960	e1a03000	mov	r3, r0
00002964	e58d3060	str	r3, [sp, #96]
00002968	e59d3060	ldr	r3, [sp, #96]
0000296c	e3530000	cmp	r3, #0	; 0x0
00002970	1a00000d	bne	0x29ac
00002974	e59f33b0	ldr	r3, [pc, #944]	; 0x2d2c
00002978	e08f3003	add	r3, pc, r3
0000297c	e5933000	ldr	r3, [r3]
00002980	e593c000	ldr	ip, [r3]
00002984	e59f33a4	ldr	r3, [pc, #932]	; 0x2d30
00002988	e08f3003	add	r3, pc, r3
0000298c	e1a00003	mov	r0, r3
00002990	e3a01001	mov	r1, #1	; 0x1
00002994	e3a02032	mov	r2, #50	; 0x32
00002998	e1a0300c	mov	r3, ip
0000299c	eb002293	bl	0xb3f0	; symbol stub for: _fwrite
000029a0	e3a03000	mov	r3, #0	; 0x0
000029a4	e58d3008	str	r3, [sp, #8]
000029a8	ea0000da	b	0x2d18
000029ac	e59d3058	ldr	r3, [sp, #88]
000029b0	e59d0060	ldr	r0, [sp, #96]
000029b4	e1a01003	mov	r1, r3
000029b8	eb0021ea	bl	0xb168	; symbol stub for: _AudioComponentInstanceNew
000029bc	e1a03000	mov	r3, r0
000029c0	e58d305c	str	r3, [sp, #92]
000029c4	e59d305c	ldr	r3, [sp, #92]
000029c8	e3530000	cmp	r3, #0	; 0x0
000029cc	0a00000c	beq	0x2a04
000029d0	e59f335c	ldr	r3, [pc, #860]	; 0x2d34
000029d4	e08f3003	add	r3, pc, r3
000029d8	e5933000	ldr	r3, [r3]
000029dc	e5933000	ldr	r3, [r3]
000029e0	e1a00003	mov	r0, r3
000029e4	e59f334c	ldr	r3, [pc, #844]	; 0x2d38
000029e8	e08f3003	add	r3, pc, r3
000029ec	e1a01003	mov	r1, r3
000029f0	e59d205c	ldr	r2, [sp, #92]
000029f4	eb002277	bl	0xb3d8	; symbol stub for: _fprintf
000029f8	e3a03000	mov	r3, #0	; 0x0
000029fc	e58d3008	str	r3, [sp, #8]
00002a00	ea0000c4	b	0x2d18
00002a04	e3a03001	mov	r3, #1	; 0x1
00002a08	e58d3040	str	r3, [sp, #64]
00002a0c	e59d3058	ldr	r3, [sp, #88]
00002a10	e5932000	ldr	r2, [r3]
00002a14	e28d3040	add	r3, sp, #64	; 0x40
00002a18	e58d3000	str	r3, [sp]
00002a1c	e3a03004	mov	r3, #4	; 0x4
00002a20	e58d3004	str	r3, [sp, #4]
00002a24	e1a00002	mov	r0, r2
00002a28	e59f130c	ldr	r1, [pc, #780]	; 0x2d3c
00002a2c	e3a02001	mov	r2, #1	; 0x1
00002a30	e3a03001	mov	r3, #1	; 0x1
00002a34	eb0021e0	bl	0xb1bc	; symbol stub for: _AudioUnitSetProperty
00002a38	e1a03000	mov	r3, r0
00002a3c	e58d305c	str	r3, [sp, #92]
00002a40	e59d305c	ldr	r3, [sp, #92]
00002a44	e3530000	cmp	r3, #0	; 0x0
00002a48	0a00000c	beq	0x2a80
00002a4c	e59f32ec	ldr	r3, [pc, #748]	; 0x2d40
00002a50	e08f3003	add	r3, pc, r3
00002a54	e5933000	ldr	r3, [r3]
00002a58	e5933000	ldr	r3, [r3]
00002a5c	e1a00003	mov	r0, r3
00002a60	e59f32dc	ldr	r3, [pc, #732]	; 0x2d44
00002a64	e08f3003	add	r3, pc, r3
00002a68	e1a01003	mov	r1, r3
00002a6c	e59d205c	ldr	r2, [sp, #92]
00002a70	eb002258	bl	0xb3d8	; symbol stub for: _fprintf
00002a74	e3a03000	mov	r3, #0	; 0x0
00002a78	e58d3008	str	r3, [sp, #8]
00002a7c	ea0000a5	b	0x2d18
00002a80	e59f32c0	ldr	r3, [pc, #704]	; 0x2d48
00002a84	e08f3003	add	r3, pc, r3
00002a88	e28de018	add	lr, sp, #24	; 0x18
00002a8c	e1a0c003	mov	ip, r3
00002a90	e8bc000f	ldmia	ip!, {r0, r1, r2, r3}
00002a94	e8ae000f	stmia	lr!, {r0, r1, r2, r3}
00002a98	e8bc000f	ldmia	ip!, {r0, r1, r2, r3}
00002a9c	e8ae000f	stmia	lr!, {r0, r1, r2, r3}
00002aa0	e89c0003	ldmia	ip, {r0, r1}
00002aa4	e88e0003	stmia	lr, {r0, r1}
00002aa8	e59d3058	ldr	r3, [sp, #88]
00002aac	e5932000	ldr	r2, [r3]
00002ab0	e28d3018	add	r3, sp, #24	; 0x18
00002ab4	e58d3000	str	r3, [sp]
00002ab8	e3a03028	mov	r3, #40	; 0x28
00002abc	e58d3004	str	r3, [sp, #4]
00002ac0	e1a00002	mov	r0, r2
00002ac4	e3a01008	mov	r1, #8	; 0x8
00002ac8	e3a02002	mov	r2, #2	; 0x2
00002acc	e3a03001	mov	r3, #1	; 0x1
00002ad0	eb0021b9	bl	0xb1bc	; symbol stub for: _AudioUnitSetProperty
00002ad4	e1a03000	mov	r3, r0
00002ad8	e58d305c	str	r3, [sp, #92]
00002adc	e59d305c	ldr	r3, [sp, #92]
00002ae0	e3530000	cmp	r3, #0	; 0x0
00002ae4	0a00000c	beq	0x2b1c
00002ae8	e59f325c	ldr	r3, [pc, #604]	; 0x2d4c
00002aec	e08f3003	add	r3, pc, r3
00002af0	e5933000	ldr	r3, [r3]
00002af4	e5933000	ldr	r3, [r3]
00002af8	e1a00003	mov	r0, r3
00002afc	e59f324c	ldr	r3, [pc, #588]	; 0x2d50
00002b00	e08f3003	add	r3, pc, r3
00002b04	e1a01003	mov	r1, r3
00002b08	e59d205c	ldr	r2, [sp, #92]
00002b0c	eb002231	bl	0xb3d8	; symbol stub for: _fprintf
00002b10	e3a03000	mov	r3, #0	; 0x0
00002b14	e58d3008	str	r3, [sp, #8]
00002b18	ea00007e	b	0x2d18
00002b1c	e59f3230	ldr	r3, [pc, #560]	; 0x2d54
00002b20	e08f3003	add	r3, pc, r3
00002b24	e5933000	ldr	r3, [r3]
00002b28	e58d3010	str	r3, [sp, #16]
00002b2c	e59d3058	ldr	r3, [sp, #88]
00002b30	e58d3014	str	r3, [sp, #20]
00002b34	e59d3058	ldr	r3, [sp, #88]
00002b38	e5932000	ldr	r2, [r3]
00002b3c	e28d3010	add	r3, sp, #16	; 0x10
00002b40	e58d3000	str	r3, [sp]
00002b44	e3a03008	mov	r3, #8	; 0x8
00002b48	e58d3004	str	r3, [sp, #4]
00002b4c	e1a00002	mov	r0, r2
00002b50	e59f1200	ldr	r1, [pc, #512]	; 0x2d58
00002b54	e3a02000	mov	r2, #0	; 0x0
00002b58	e3a03000	mov	r3, #0	; 0x0
00002b5c	eb002196	bl	0xb1bc	; symbol stub for: _AudioUnitSetProperty
00002b60	e1a03000	mov	r3, r0
00002b64	e58d305c	str	r3, [sp, #92]
00002b68	e59d305c	ldr	r3, [sp, #92]
00002b6c	e3530000	cmp	r3, #0	; 0x0
00002b70	0a00000c	beq	0x2ba8
00002b74	e59f31e0	ldr	r3, [pc, #480]	; 0x2d5c
00002b78	e08f3003	add	r3, pc, r3
00002b7c	e5933000	ldr	r3, [r3]
00002b80	e5933000	ldr	r3, [r3]
00002b84	e1a00003	mov	r0, r3
00002b88	e59f31d0	ldr	r3, [pc, #464]	; 0x2d60
00002b8c	e08f3003	add	r3, pc, r3
00002b90	e1a01003	mov	r1, r3
00002b94	e59d205c	ldr	r2, [sp, #92]
00002b98	eb00220e	bl	0xb3d8	; symbol stub for: _fprintf
00002b9c	e3a03000	mov	r3, #0	; 0x0
00002ba0	e58d3008	str	r3, [sp, #8]
00002ba4	ea00005b	b	0x2d18
00002ba8	e59d3058	ldr	r3, [sp, #88]
00002bac	e5933000	ldr	r3, [r3]
00002bb0	e1a00003	mov	r0, r3
00002bb4	eb00217a	bl	0xb1a4	; symbol stub for: _AudioUnitInitialize
00002bb8	e1a03000	mov	r3, r0
00002bbc	e58d305c	str	r3, [sp, #92]
00002bc0	e59d305c	ldr	r3, [sp, #92]
00002bc4	e3530000	cmp	r3, #0	; 0x0
00002bc8	0a00000c	beq	0x2c00
00002bcc	e59f3190	ldr	r3, [pc, #400]	; 0x2d64
00002bd0	e08f3003	add	r3, pc, r3
00002bd4	e5933000	ldr	r3, [r3]
00002bd8	e5933000	ldr	r3, [r3]
00002bdc	e1a00003	mov	r0, r3
00002be0	e59f3180	ldr	r3, [pc, #384]	; 0x2d68
00002be4	e08f3003	add	r3, pc, r3
00002be8	e1a01003	mov	r1, r3
00002bec	e59d205c	ldr	r2, [sp, #92]
00002bf0	eb0021f8	bl	0xb3d8	; symbol stub for: _fprintf
00002bf4	e3a03000	mov	r3, #0	; 0x0
00002bf8	e58d3008	str	r3, [sp, #8]
00002bfc	ea000045	b	0x2d18
00002c00	e3a00000	mov	r0, #0	; 0x0
00002c04	e3a01000	mov	r1, #0	; 0x0
00002c08	e59f315c	ldr	r3, [pc, #348]	; 0x2d6c
00002c0c	e08f3003	add	r3, pc, r3
00002c10	e5933000	ldr	r3, [r3]
00002c14	e1a02003	mov	r2, r3
00002c18	e59d3058	ldr	r3, [sp, #88]
00002c1c	eb00215a	bl	0xb18c	; symbol stub for: _AudioSessionInitialize
00002c20	e1a03000	mov	r3, r0
00002c24	e58d305c	str	r3, [sp, #92]
00002c28	e59d305c	ldr	r3, [sp, #92]
00002c2c	e3530000	cmp	r3, #0	; 0x0
00002c30	0a00000c	beq	0x2c68
00002c34	e59f3134	ldr	r3, [pc, #308]	; 0x2d70
00002c38	e08f3003	add	r3, pc, r3
00002c3c	e5933000	ldr	r3, [r3]
00002c40	e5933000	ldr	r3, [r3]
00002c44	e1a00003	mov	r0, r3
00002c48	e59f3124	ldr	r3, [pc, #292]	; 0x2d74
00002c4c	e08f3003	add	r3, pc, r3
00002c50	e1a01003	mov	r1, r3
00002c54	e59d205c	ldr	r2, [sp, #92]
00002c58	eb0021de	bl	0xb3d8	; symbol stub for: _fprintf
00002c5c	e3a03000	mov	r3, #0	; 0x0
00002c60	e58d3008	str	r3, [sp, #8]
00002c64	ea00002b	b	0x2d18
00002c68	e3a00001	mov	r0, #1	; 0x1
00002c6c	eb002149	bl	0xb198	; symbol stub for: _AudioSessionSetActive
00002c70	e1a03000	mov	r3, r0
00002c74	e58d305c	str	r3, [sp, #92]
00002c78	e59d305c	ldr	r3, [sp, #92]
00002c7c	e3530000	cmp	r3, #0	; 0x0
00002c80	0a00000c	beq	0x2cb8
00002c84	e59f30ec	ldr	r3, [pc, #236]	; 0x2d78
00002c88	e08f3003	add	r3, pc, r3
00002c8c	e5933000	ldr	r3, [r3]
00002c90	e5933000	ldr	r3, [r3]
00002c94	e1a00003	mov	r0, r3
00002c98	e59f30dc	ldr	r3, [pc, #220]	; 0x2d7c
00002c9c	e08f3003	add	r3, pc, r3
00002ca0	e1a01003	mov	r1, r3
00002ca4	e59d205c	ldr	r2, [sp, #92]
00002ca8	eb0021ca	bl	0xb3d8	; symbol stub for: _fprintf
00002cac	e3a03000	mov	r3, #0	; 0x0
00002cb0	e58d3008	str	r3, [sp, #8]
00002cb4	ea000017	b	0x2d18
00002cb8	e59d3058	ldr	r3, [sp, #88]
00002cbc	e5933000	ldr	r3, [r3]
00002cc0	e1a00003	mov	r0, r3
00002cc4	eb00212a	bl	0xb174	; symbol stub for: _AudioOutputUnitStart
00002cc8	e1a03000	mov	r3, r0
00002ccc	e58d305c	str	r3, [sp, #92]
00002cd0	e59d305c	ldr	r3, [sp, #92]
00002cd4	e3530000	cmp	r3, #0	; 0x0
00002cd8	0a00000c	beq	0x2d10
00002cdc	e59f309c	ldr	r3, [pc, #156]	; 0x2d80
00002ce0	e08f3003	add	r3, pc, r3
00002ce4	e5933000	ldr	r3, [r3]
00002ce8	e5933000	ldr	r3, [r3]
00002cec	e1a00003	mov	r0, r3
00002cf0	e59f308c	ldr	r3, [pc, #140]	; 0x2d84
00002cf4	e08f3003	add	r3, pc, r3
00002cf8	e1a01003	mov	r1, r3
00002cfc	e59d205c	ldr	r2, [sp, #92]
00002d00	eb0021b4	bl	0xb3d8	; symbol stub for: _fprintf
00002d04	e3a03000	mov	r3, #0	; 0x0
00002d08	e58d3008	str	r3, [sp, #8]
00002d0c	ea000001	b	0x2d18
00002d10	e3a03001	mov	r3, #1	; 0x1
00002d14	e58d3008	str	r3, [sp, #8]
00002d18	e59d3008	ldr	r3, [sp, #8]
00002d1c	e1a00003	mov	r0, r3
00002d20	e247d000	sub	sp, r7, #0	; 0x0
00002d24	e8bd8080	ldmia	sp!, {r7, pc}
00002d28	00009378	andeq	r9, r0, r8, ror r3
00002d2c	0000968c	andeq	r9, r0, ip, lsl #13
00002d30	00008b34	andeq	r8, r0, r4, lsr r11
00002d34	00009630	andeq	r9, r0, r0, lsr r6
00002d38	00008b08	andeq	r8, r0, r8, lsl #22
00002d3c	000007d3	ldreqd	r0, [r0], -r3
00002d40	000095b4	streqh	r9, [r0], -r4
00002d44	00008ad0	ldreqd	r8, [r0], -r0
00002d48	00009200	andeq	r9, r0, r0, lsl #4
00002d4c	00009518	andeq	r9, r0, r8, lsl r5
00002d50	00008ac0	andeq	r8, r0, r0, asr #21
00002d54	00009598	muleq	r0, r8, r5
00002d58	000007d5	ldreqd	r0, [r0], -r5
00002d5c	0000948c	andeq	r9, r0, ip, lsl #9
00002d60	00008ad0	ldreqd	r8, [r0], -r0
00002d64	00009434	andeq	r9, r0, r4, lsr r4
00002d68	00008b0c	andeq	r8, r0, ip, lsl #22
00002d6c	000094a8	andeq	r9, r0, r8, lsr #9
00002d70	000093cc	andeq	r9, r0, ip, asr #7
00002d74	00008ad4	ldreqd	r8, [r0], -r4
00002d78	0000937c	andeq	r9, r0, ip, ror r3
00002d7c	00008ad8	ldreqd	r8, [r0], -r8
00002d80	00009324	andeq	r9, r0, r4, lsr #6
00002d84	00008aa8	andeq	r8, r0, r8, lsr #21
_audioInputCallback:
00002d88	e92d4090	stmdb	sp!, {r4, r7, lr}
00002d8c	e28d7004	add	r7, sp, #4	; 0x4
00002d90	e24dd034	sub	sp, sp, #52	; 0x34
00002d94	e5070020	str	r0, [r7, #-32]
00002d98	e5071024	str	r1, [r7, #-36]
00002d9c	e5072028	str	r2, [r7, #-40]
00002da0	e507302c	str	r3, [r7, #-44]
00002da4	e1a0300d	mov	r3, sp
00002da8	e1a04003	mov	r4, r3
00002dac	e5173020	ldr	r3, [r7, #-32]
00002db0	e5073008	str	r3, [r7, #-8]
00002db4	e5973008	ldr	r3, [r7, #8]
00002db8	e1a03083	mov	r3, r3, lsl #1
00002dbc	e2833003	add	r3, r3, #3	; 0x3
00002dc0	e2833003	add	r3, r3, #3	; 0x3
00002dc4	e1a03123	mov	r3, r3, lsr #2
00002dc8	e1a03103	mov	r3, r3, lsl #2
00002dcc	e063d00d	rsb	sp, r3, sp
00002dd0	e28d2008	add	r2, sp, #8	; 0x8
00002dd4	e5072030	str	r2, [r7, #-48]
00002dd8	e5172030	ldr	r2, [r7, #-48]
00002ddc	e2823003	add	r3, r2, #3	; 0x3
00002de0	e1a03123	mov	r3, r3, lsr #2
00002de4	e1a03103	mov	r3, r3, lsl #2
00002de8	e5073030	str	r3, [r7, #-48]
00002dec	e5173030	ldr	r3, [r7, #-48]
00002df0	e507300c	str	r3, [r7, #-12]
00002df4	e3a03001	mov	r3, #1	; 0x1
00002df8	e507301c	str	r3, [r7, #-28]
00002dfc	e517300c	ldr	r3, [r7, #-12]
00002e00	e5073010	str	r3, [r7, #-16]
00002e04	e5973008	ldr	r3, [r7, #8]
00002e08	e1a03083	mov	r3, r3, lsl #1
00002e0c	e5073014	str	r3, [r7, #-20]
00002e10	e3a03001	mov	r3, #1	; 0x1
00002e14	e5073018	str	r3, [r7, #-24]
00002e18	e5173008	ldr	r3, [r7, #-8]
00002e1c	e5932000	ldr	r2, [r3]
00002e20	e5973008	ldr	r3, [r7, #8]
00002e24	e58d3000	str	r3, [sp]
00002e28	e247301c	sub	r3, r7, #28	; 0x1c
00002e2c	e58d3004	str	r3, [sp, #4]
00002e30	e1a00002	mov	r0, r2
00002e34	e5171024	ldr	r1, [r7, #-36]
00002e38	e5172028	ldr	r2, [r7, #-40]
00002e3c	e3a03001	mov	r3, #1	; 0x1
00002e40	eb0020da	bl	0xb1b0	; symbol stub for: _AudioUnitRender
00002e44	e5173008	ldr	r3, [r7, #-8]
00002e48	e5932004	ldr	r2, [r3, #4]
00002e4c	e517100c	ldr	r1, [r7, #-12]
00002e50	e5973008	ldr	r3, [r7, #8]
00002e54	e1a03083	mov	r3, r3, lsl #1
00002e58	e1a00002	mov	r0, r2
00002e5c	e1a02003	mov	r2, r3
00002e60	eb001b44	bl	_dsp_add_microphone_sample
00002e64	e3a03000	mov	r3, #0	; 0x0
00002e68	e1a0d004	mov	sp, r4
00002e6c	e1a00003	mov	r0, r3
00002e70	e247d004	sub	sp, r7, #4	; 0x4
00002e74	e8bd8090	ldmia	sp!, {r4, r7, pc}
_audioInterruptionListener:
00002e78	e92d4080	stmdb	sp!, {r7, lr}
00002e7c	e28d7000	add	r7, sp, #0	; 0x0
00002e80	e24dd00c	sub	sp, sp, #12	; 0xc
00002e84	e58d0004	str	r0, [sp, #4]
00002e88	e58d1000	str	r1, [sp]
00002e8c	e59d3004	ldr	r3, [sp, #4]
00002e90	e58d3008	str	r3, [sp, #8]
00002e94	e59d3000	ldr	r3, [sp]
00002e98	e3530001	cmp	r3, #1	; 0x1
00002e9c	1a000011	bne	0x2ee8
00002ea0	e59d3008	ldr	r3, [sp, #8]
00002ea4	e5933000	ldr	r3, [r3]
00002ea8	e1a00003	mov	r0, r3
00002eac	eb0020b3	bl	0xb180	; symbol stub for: _AudioOutputUnitStop
00002eb0	e3a00000	mov	r0, #0	; 0x0
00002eb4	eb0020b7	bl	0xb198	; symbol stub for: _AudioSessionSetActive
00002eb8	e59f3080	ldr	r3, [pc, #128]	; 0x2f40
00002ebc	e08f3003	add	r3, pc, r3
00002ec0	e5933000	ldr	r3, [r3]
00002ec4	e593c000	ldr	ip, [r3]
00002ec8	e59f3074	ldr	r3, [pc, #116]	; 0x2f44
00002ecc	e08f3003	add	r3, pc, r3
00002ed0	e1a00003	mov	r0, r3
00002ed4	e3a01001	mov	r1, #1	; 0x1
00002ed8	e3a0201e	mov	r2, #30	; 0x1e
00002edc	e1a0300c	mov	r3, ip
00002ee0	eb002142	bl	0xb3f0	; symbol stub for: _fwrite
00002ee4	ea000013	b	0x2f38
00002ee8	e59d3000	ldr	r3, [sp]
00002eec	e3530000	cmp	r3, #0	; 0x0
00002ef0	1a000010	bne	0x2f38
00002ef4	e3a00001	mov	r0, #1	; 0x1
00002ef8	eb0020a6	bl	0xb198	; symbol stub for: _AudioSessionSetActive
00002efc	e59d3008	ldr	r3, [sp, #8]
00002f00	e5933000	ldr	r3, [r3]
00002f04	e1a00003	mov	r0, r3
00002f08	eb002099	bl	0xb174	; symbol stub for: _AudioOutputUnitStart
00002f0c	e59f3034	ldr	r3, [pc, #52]	; 0x2f48
00002f10	e08f3003	add	r3, pc, r3
00002f14	e5933000	ldr	r3, [r3]
00002f18	e593c000	ldr	ip, [r3]
00002f1c	e59f3028	ldr	r3, [pc, #40]	; 0x2f4c
00002f20	e08f3003	add	r3, pc, r3
00002f24	e1a00003	mov	r0, r3
00002f28	e3a01001	mov	r1, #1	; 0x1
00002f2c	e3a0201c	mov	r2, #28	; 0x1c
00002f30	e1a0300c	mov	r3, ip
00002f34	eb00212d	bl	0xb3f0	; symbol stub for: _fwrite
00002f38	e247d000	sub	sp, r7, #0	; 0x0
00002f3c	e8bd8080	ldmia	sp!, {r7, pc}
00002f40	00009148	andeq	r9, r0, r8, asr #2
00002f44	00008904	andeq	r8, r0, r4, lsl #18
00002f48	000090f4	streqd	r9, [r0], -r4
00002f4c	000088d0	ldreqd	r8, [r0], -r0
_initializeLocation:
00002f50	e92d40f0	stmdb	sp!, {r4, r5, r6, r7, lr}
00002f54	e28d700c	add	r7, sp, #12	; 0xc
00002f58	e24dd020	sub	sp, sp, #32	; 0x20
00002f5c	e58d0004	str	r0, [sp, #4]
00002f60	e3a00008	mov	r0, #8	; 0x8
00002f64	eb00212d	bl	0xb420	; symbol stub for: _malloc
00002f68	e1a03000	mov	r3, r0
00002f6c	e58d3018	str	r3, [sp, #24]
00002f70	e59d2018	ldr	r2, [sp, #24]
00002f74	e59d3004	ldr	r3, [sp, #4]
00002f78	e5823000	str	r3, [r2]
00002f7c	eb002091	bl	0xb1c8	; symbol stub for: _CFBundleGetMainBundle
00002f80	e1a03000	mov	r3, r0
00002f84	e2833010	add	r3, r3, #16	; 0x10
00002f88	e58d301c	str	r3, [sp, #28]
00002f8c	e59d301c	ldr	r3, [sp, #28]
00002f90	e3530000	cmp	r3, #0	; 0x0
00002f94	0a000009	beq	0x2fc0
00002f98	e59d301c	ldr	r3, [sp, #28]
00002f9c	e5933000	ldr	r3, [r3]
00002fa0	e3a00000	mov	r0, #0	; 0x0
00002fa4	e3a01000	mov	r1, #0	; 0x0
00002fa8	e1a02003	mov	r2, r3
00002fac	eb00208b	bl	0xb1e0	; symbol stub for: _CFDictionaryCreateMutableCopy
00002fb0	e1a02000	mov	r2, r0
00002fb4	e59d301c	ldr	r3, [sp, #28]
00002fb8	e5832000	str	r2, [r3]
00002fbc	ea00000c	b	0x2ff4
00002fc0	e3a00000	mov	r0, #0	; 0x0
00002fc4	e3a01000	mov	r1, #0	; 0x0
00002fc8	e59f30e8	ldr	r3, [pc, #232]	; 0x30b8
00002fcc	e08f3003	add	r3, pc, r3
00002fd0	e5933000	ldr	r3, [r3]
00002fd4	e1a02003	mov	r2, r3
00002fd8	e59f30dc	ldr	r3, [pc, #220]	; 0x30bc
00002fdc	e08f3003	add	r3, pc, r3
00002fe0	e5933000	ldr	r3, [r3]
00002fe4	eb00207a	bl	0xb1d4	; symbol stub for: _CFDictionaryCreateMutable
00002fe8	e1a02000	mov	r2, r0
00002fec	e59d301c	ldr	r3, [sp, #28]
00002ff0	e5832000	str	r2, [r3]
00002ff4	e59d301c	ldr	r3, [sp, #28]
00002ff8	e5932000	ldr	r2, [r3]
00002ffc	e59f30bc	ldr	r3, [pc, #188]	; 0x30c0
00003000	e08f3003	add	r3, pc, r3
00003004	e1a01003	mov	r1, r3
00003008	e59f30b4	ldr	r3, [pc, #180]	; 0x30c4
0000300c	e08f3003	add	r3, pc, r3
00003010	e1a00002	mov	r0, r2
00003014	e1a02003	mov	r2, r3
00003018	eb002073	bl	0xb1ec	; symbol stub for: _CFDictionarySetValue
0000301c	e59f30a4	ldr	r3, [pc, #164]	; 0x30c8
00003020	e08f3003	add	r3, pc, r3
00003024	e28dc008	add	ip, sp, #8	; 0x8
00003028	e893000f	ldmia	r3, {r0, r1, r2, r3}
0000302c	e88c000f	stmia	ip, {r0, r1, r2, r3}
00003030	e28d2008	add	r2, sp, #8	; 0x8
00003034	e3a00000	mov	r0, #0	; 0x0
00003038	e59f308c	ldr	r3, [pc, #140]	; 0x30cc
0000303c	e08f3003	add	r3, pc, r3
00003040	e5933000	ldr	r3, [r3]
00003044	e1a01003	mov	r1, r3
00003048	eb002070	bl	0xb210	; symbol stub for: _CLClientCreate
0000304c	e1a02000	mov	r2, r0
00003050	e59d3018	ldr	r3, [sp, #24]
00003054	e5832004	str	r2, [r3, #4]
00003058	eb002066	bl	0xb1f8	; symbol stub for: _CFRunLoopGetMain
0000305c	e1a03000	mov	r3, r0
00003060	e1a00003	mov	r0, r3
00003064	eb002072	bl	0xb234	; symbol stub for: _CLCommonSetRunLoop
00003068	e59d3018	ldr	r3, [sp, #24]
0000306c	e5931004	ldr	r1, [r3, #4]
00003070	e59f3058	ldr	r3, [pc, #88]	; 0x30d0
00003074	e08f3003	add	r3, pc, r3
00003078	e5933000	ldr	r3, [r3]
0000307c	e8930060	ldmia	r3, {r5, r6}
00003080	e59f304c	ldr	r3, [pc, #76]	; 0x30d4
00003084	e08f3003	add	r3, pc, r3
00003088	e5933000	ldr	r3, [r3]
0000308c	e8930018	ldmia	r3, {r3, r4}
00003090	e1a02004	mov	r2, r4
00003094	e58d2000	str	r2, [sp]
00003098	e1a00001	mov	r0, r1
0000309c	e1a01005	mov	r1, r5
000030a0	e1a02006	mov	r2, r6
000030a4	eb00205f	bl	0xb228	; symbol stub for: _CLClientStartLocationUpdates
000030a8	e3a03001	mov	r3, #1	; 0x1
000030ac	e1a00003	mov	r0, r3
000030b0	e247d00c	sub	sp, r7, #12	; 0xc
000030b4	e8bd80f0	ldmia	sp!, {r4, r5, r6, r7, pc}
000030b8	0000903c	andeq	r9, r0, ip, lsr r0
000030bc	00009030	andeq	r9, r0, r0, lsr r0
000030c0	00009244	andeq	r9, r0, r4, asr #4
000030c4	00009248	andeq	r9, r0, r8, asr #4
000030c8	00008ca0	andeq	r8, r0, r0, lsr #25
000030cc	00009080	andeq	r9, r0, r0, lsl #1
000030d0	00008fa0	andeq	r8, r0, r0, lsr #31
000030d4	00008f8c	andeq	r8, r0, ip, lsl #31
_handleLocationEvent:
000030d8	e92d40f0	stmdb	sp!, {r4, r5, r6, r7, lr}
000030dc	e28d700c	add	r7, sp, #12	; 0xc
000030e0	e92d0c00	stmdb	sp!, {r10, r11}
000030e4	e24dd074	sub	sp, sp, #116	; 0x74
000030e8	e58d0024	str	r0, [sp, #36]
000030ec	e58d1020	str	r1, [sp, #32]
000030f0	e58d201c	str	r2, [sp, #28]
000030f4	e59f3098	ldr	r3, [pc, #152]	; 0x3194
000030f8	e08f3003	add	r3, pc, r3
000030fc	e5933000	ldr	r3, [r3]
00003100	e5933000	ldr	r3, [r3]
00003104	e1a00003	mov	r0, r3
00003108	e59f3088	ldr	r3, [pc, #136]	; 0x3198
0000310c	e08f3003	add	r3, pc, r3
00003110	e1a01003	mov	r1, r3
00003114	e59d201c	ldr	r2, [sp, #28]
00003118	eb0020ae	bl	0xb3d8	; symbol stub for: _fprintf
0000311c	e59d3020	ldr	r3, [sp, #32]
00003120	e3530000	cmp	r3, #0	; 0x0
00003124	1a000017	bne	0x3188
00003128	e28d3028	add	r3, sp, #40	; 0x28
0000312c	e59d0024	ldr	r0, [sp, #36]
00003130	e1a01003	mov	r1, r3
00003134	eb002038	bl	0xb21c	; symbol stub for: _CLClientGetLocation
00003138	ed9d7b0b	fldd	d7, [sp, #44]
0000313c	e28da034	add	r10, sp, #52	; 0x34
00003140	e89a0c00	ldmia	r10, {r10, r11}
00003144	e28d3044	add	r3, sp, #68	; 0x44
00003148	e8930018	ldmia	r3, {r3, r4}
0000314c	e28d103c	add	r1, sp, #60	; 0x3c
00003150	e8910006	ldmia	r1, {r1, r2}
00003154	e28d504c	add	r5, sp, #76	; 0x4c
00003158	e8950060	ldmia	r5, {r5, r6}
0000315c	e98d0018	stmib	sp, {r3, r4}
00003160	e58d100c	str	r1, [sp, #12]
00003164	e58d2010	str	r2, [sp, #16]
00003168	e58d5014	str	r5, [sp, #20]
0000316c	e58d6018	str	r6, [sp, #24]
00003170	e1a0300b	mov	r3, r11
00003174	e58d3000	str	r3, [sp]
00003178	e1a0300a	mov	r3, r10
0000317c	e3a00000	mov	r0, #0	; 0x0
00003180	ec521b17	fmrrd	r1, r2, d7
00003184	eb001b22	bl	_dsp_add_location_sample
00003188	e247d014	sub	sp, r7, #20	; 0x14
0000318c	e8bd0c00	ldmia	sp!, {r10, r11}
00003190	e8bd80f0	ldmia	sp!, {r4, r5, r6, r7, pc}
00003194	00008f0c	andeq	r8, r0, ip, lsl #30
00003198	0000872c	andeq	r8, r0, ip, lsr #14
_initializeAccelerometer:
0000319c	e92d4080	stmdb	sp!, {r7, lr}
000031a0	e28d7000	add	r7, sp, #0	; 0x0
000031a4	e24dd008	sub	sp, sp, #8	; 0x8
000031a8	e58d0000	str	r0, [sp]
000031ac	e3a00008	mov	r0, #8	; 0x8
000031b0	eb00209a	bl	0xb420	; symbol stub for: _malloc
000031b4	e1a03000	mov	r3, r0
000031b8	e58d3004	str	r3, [sp, #4]
000031bc	e59d2004	ldr	r2, [sp, #4]
000031c0	e59d3000	ldr	r3, [sp]
000031c4	e5823004	str	r3, [r2, #4]
000031c8	e3a00000	mov	r0, #0	; 0x0
000031cc	eb002021	bl	0xb258	; symbol stub for: _IOHIDEventSystemCreate
000031d0	e1a02000	mov	r2, r0
000031d4	e59d3004	ldr	r3, [sp, #4]
000031d8	e5832000	str	r2, [r3]
000031dc	e59d3004	ldr	r3, [sp, #4]
000031e0	e5933000	ldr	r3, [r3]
000031e4	e1a00003	mov	r0, r3
000031e8	e59f3024	ldr	r3, [pc, #36]	; 0x3214
000031ec	e08f3003	add	r3, pc, r3
000031f0	e5933000	ldr	r3, [r3]
000031f4	e1a01003	mov	r1, r3
000031f8	e59d2004	ldr	r2, [sp, #4]
000031fc	e3a03000	mov	r3, #0	; 0x0
00003200	eb002017	bl	0xb264	; symbol stub for: _IOHIDEventSystemOpen
00003204	e3a03001	mov	r3, #1	; 0x1
00003208	e1a00003	mov	r0, r3
0000320c	e247d000	sub	sp, r7, #0	; 0x0
00003210	e8bd8080	ldmia	sp!, {r7, pc}
00003214	00008ed4	ldreqd	r8, [r0], -r4
_handleHIDAccelerometerEvent:
00003218	e92d4080	stmdb	sp!, {r7, lr}
0000321c	e28d7000	add	r7, sp, #0	; 0x0
00003220	e24dd02c	sub	sp, sp, #44	; 0x2c
00003224	e58d000c	str	r0, [sp, #12]
00003228	e58d1008	str	r1, [sp, #8]
0000322c	e58d2004	str	r2, [sp, #4]
00003230	e58d3000	str	r3, [sp]
00003234	e59d300c	ldr	r3, [sp, #12]
00003238	e58d3010	str	r3, [sp, #16]
0000323c	e59d0000	ldr	r0, [sp]
00003240	eb002001	bl	0xb24c	; symbol stub for: _IOHIDEventGetType
00003244	e1a03000	mov	r3, r0
00003248	e353000d	cmp	r3, #13	; 0xd
0000324c	1a00001e	bne	0x32cc
00003250	e59d0000	ldr	r0, [sp]
00003254	e3a0180d	mov	r1, #851968	; 0xd0000
00003258	eb001ff8	bl	0xb240	; symbol stub for: _IOHIDEventGetFloatValue
0000325c	ee070a90	fmsr	s15, r0
00003260	eeb77ae7	fcvtds	d7, s15
00003264	ed8d7b05	fstd	d7, [sp, #20]
00003268	e59d0000	ldr	r0, [sp]
0000326c	e59f1060		ldr	r1, [pc, #96]	; 0x32d4
00003270	eb001ff2	bl	0xb240	; symbol stub for: _IOHIDEventGetFloatValue
00003274	ee070a90	fmsr	s15, r0
00003278	eeb77ae7	fcvtds	d7, s15
0000327c	ed8d7b07	fstd	d7, [sp, #28]
00003280	e59d0000	ldr	r0, [sp]
00003284	e59f104c	ldr	r1, [pc, #76]	; 0x32d8
00003288	eb001fec	bl	0xb240	; symbol stub for: _IOHIDEventGetFloatValue
0000328c	ee070a90	fmsr	s15, r0
00003290	eeb77ae7	fcvtds	d7, s15
00003294	ed8d7b09	fstd	d7, [sp, #36]
00003298	e59d3010	ldr	r3, [sp, #16]
0000329c	e5933004	ldr	r3, [r3, #4]
000032a0	ed9d7b05	fldd	d7, [sp, #20]
000032a4	eeb76bc7	fcvtsd	s12, d7
000032a8	ed9d7b07	fldd	d7, [sp, #28]
000032ac	eef76bc7	fcvtsd	s13, d7
000032b0	ed9d7b09	fldd	d7, [sp, #36]
000032b4	eef77bc7	fcvtsd	s15, d7
000032b8	e1a00003	mov	r0, r3
000032bc	ee161a10	fmrs	r1, s12
000032c0	ee162a90	fmrs	r2, s13
000032c4	ee173a90	fmrs	r3, s15
000032c8	eb001b9e	bl	_dsp_add_accelerometer_sample
000032cc	e247d000	sub	sp, r7, #0	; 0x0
000032d0	e8bd8080	ldmia	sp!, {r7, pc}
000032d4	000d0001	andeq	r0, sp, r1
000032d8	000d0002	andeq	r0, sp, r2
_main:
000032dc	e92d4080	stmdb	sp!, {r7, lr}
000032e0	e28d7000	add	r7, sp, #0	; 0x0
000032e4	e24dd00c	sub	sp, sp, #12	; 0xc
000032e8	e58d0004	str	r0, [sp, #4]
000032ec	e58d1000	str	r1, [sp]
000032f0	e59f3048	ldr	r3, [pc, #72]	; 0x3340
000032f4	e08f3003	add	r3, pc, r3
000032f8	e1a00003	mov	r0, r3
000032fc	e59f3040	ldr	r3, [pc, #64]	; 0x3344
00003300	e08f3003	add	r3, pc, r3
00003304	e1a01003	mov	r1, r3
00003308	eb001c43	bl	_dsp_init
0000330c	e1a03000	mov	r3, r0
00003310	e58d3008	str	r3, [sp, #8]
00003314	e59d0008	ldr	r0, [sp, #8]
00003318	ebfffd79	bl	_initializeAudio
0000331c	e59d0008	ldr	r0, [sp, #8]
00003320	ebffff0a	bl	_initializeLocation
00003324	e59d0008	ldr	r0, [sp, #8]
00003328	ebffff9b	bl	_initializeAccelerometer
0000332c	eb001fb4	bl	0xb204	; symbol stub for: _CFRunLoopRun
00003330	e3a03000	mov	r3, #0	; 0x0
00003334	e1a00003	mov	r0, r3
00003338	e247d000	sub	sp, r7, #0	; 0x0
0000333c	e8bd8080	ldmia	sp!, {r7, pc}
00003340	0000856c	andeq	r8, r0, ip, ror #10
00003344	0000856c	andeq	r8, r0, ip, ror #10
__ZN3dsp6Header21InitAsDefaultInstanceEv:
00003348	e92d4080	stmdb	sp!, {r7, lr}
0000334c	e28d7000	add	r7, sp, #0	; 0x0
00003350	e24dd004	sub	sp, sp, #4	; 0x4
00003354	e58d0000	str	r0, [sp]
00003358	e247d000	sub	sp, r7, #0	; 0x0
0000335c	e8bd8080	ldmia	sp!, {r7, pc}
__ZN3dsp24AccelerometerDescription21InitAsDefaultInstanceEv:
00003360	e92d4080	stmdb	sp!, {r7, lr}
00003364	e28d7000	add	r7, sp, #0	; 0x0
00003368	e24dd004	sub	sp, sp, #4	; 0x4
0000336c	e58d0000	str	r0, [sp]
00003370	e247d000	sub	sp, r7, #0	; 0x0
00003374	e8bd8080	ldmia	sp!, {r7, pc}
__ZN3dsp20AccelerometerMessage21InitAsDefaultInstanceEv:
00003378	e92d4080	stmdb	sp!, {r7, lr}
0000337c	e28d7000	add	r7, sp, #0	; 0x0
00003380	e24dd004	sub	sp, sp, #4	; 0x4
00003384	e58d0000	str	r0, [sp]
00003388	e247d000	sub	sp, r7, #0	; 0x0
0000338c	e8bd8080	ldmia	sp!, {r7, pc}
__ZN3dsp21MicrophoneDescription21InitAsDefaultInstanceEv:
00003390	e92d4080	stmdb	sp!, {r7, lr}
00003394	e28d7000	add	r7, sp, #0	; 0x0
00003398	e24dd004	sub	sp, sp, #4	; 0x4
0000339c	e58d0000	str	r0, [sp]
000033a0	e247d000	sub	sp, r7, #0	; 0x0
000033a4	e8bd8080	ldmia	sp!, {r7, pc}
__ZN3dsp17MicrophoneMessage21InitAsDefaultInstanceEv:
000033a8	e92d4080	stmdb	sp!, {r7, lr}
000033ac	e28d7000	add	r7, sp, #0	; 0x0
000033b0	e24dd004	sub	sp, sp, #4	; 0x4
000033b4	e58d0000	str	r0, [sp]
000033b8	e247d000	sub	sp, r7, #0	; 0x0
000033bc	e8bd8080	ldmia	sp!, {r7, pc}
__ZN3dsp19LocationDescription21InitAsDefaultInstanceEv:
000033c0	e92d4080	stmdb	sp!, {r7, lr}
000033c4	e28d7000	add	r7, sp, #0	; 0x0
000033c8	e24dd004	sub	sp, sp, #4	; 0x4
000033cc	e58d0000	str	r0, [sp]
000033d0	e247d000	sub	sp, r7, #0	; 0x0
000033d4	e8bd8080	ldmia	sp!, {r7, pc}
__ZN3dsp15LocationMessage21InitAsDefaultInstanceEv:
000033d8	e92d4080	stmdb	sp!, {r7, lr}
000033dc	e28d7000	add	r7, sp, #0	; 0x0
000033e0	e24dd004	sub	sp, sp, #4	; 0x4
000033e4	e58d0000	str	r0, [sp]
000033e8	e247d000	sub	sp, r7, #0	; 0x0
000033ec	e8bd8080	ldmia	sp!, {r7, pc}
__ZN3dsp30protobuf_BuildDesc_dsp_2eprotoEv:
000033f0	e92d4080	stmdb	sp!, {r7, lr}
000033f4	e28d7000	add	r7, sp, #0	; 0x0
000033f8	e24dd004	sub	sp, sp, #4	; 0x4
000033fc	e59f306c	ldr	r3, [pc, #108]	; 0x3470
00003400	e08f3003	add	r3, pc, r3
00003404	e5d33000	ldrb	r3, [r3]
00003408	e3530000	cmp	r3, #0	; 0x0
0000340c	1a000015	bne	0x3468
00003410	e59f305c	ldr	r3, [pc, #92]	; 0x3474
00003414	e08f3003	add	r3, pc, r3
00003418	e1a02003	mov	r2, r3
0000341c	e3a03001	mov	r3, #1	; 0x1
00003420	e5c23000	strb	r3, [r2]
00003424	e59f004c	ldr	r0, [pc, #76]	; 0x3478
00003428	e59f1048	ldr	r1, [pc, #72]	; 0x3478
0000342c	e59f3048	ldr	r3, [pc, #72]	; 0x347c
00003430	e08f3003	add	r3, pc, r3
00003434	e1a02003	mov	r2, r3
00003438	eb001fad	bl	0xb2f4	; symbol stub for: __ZN6google8protobuf8internal13VerifyVersionEiiPKc
0000343c	eb001f97	bl	0xb2a0	; symbol stub for: __ZN6google8protobuf14DescriptorPool23internal_generated_poolEv
00003440	e1a03000	mov	r3, r0
00003444	e58d3000	str	r3, [sp]
00003448	e59d0000	ldr	r0, [sp]
0000344c	e59f302c	ldr	r3, [pc, #44]	; 0x3480
00003450	e08f3003	add	r3, pc, r3
00003454	e1a01003	mov	r1, r3
00003458	e3a02e32	mov	r2, #800	; 0x320
0000345c	e59f3020	ldr	r3, [pc, #32]	; 0x3484
00003460	e08f3003	add	r3, pc, r3
00003464	eb001f90	bl	0xb2ac	; symbol stub for: __ZN6google8protobuf14DescriptorPool26InternalBuildGeneratedFileEPKviPFvPKNS0_14FileDescriptorEE
00003468	e247d000	sub	sp, r7, #0	; 0x0
0000346c	e8bd8080	ldmia	sp!, {r7, pc}
00003470	000092e1	andeq	r9, r0, r1, ror #5
00003474	000092cd	andeq	r9, r0, sp, asr #5
00003478	001e8483	andeqs	r8, lr, r3, lsl #9
0000347c	00008514	andeq	r8, r0, r4, lsl r5
00003480	00008500	andeq	r8, r0, r0, lsl #10
00003484	00000a40	andeq	r0, r0, r0, asr #20
__ZN3dsp22messageType_descriptorEv:
00003488	e92d4080	stmdb	sp!, {r7, lr}
0000348c	e28d7000	add	r7, sp, #0	; 0x0
00003490	e59f3024	ldr	r3, [pc, #36]	; 0x34bc
00003494	e08f3003	add	r3, pc, r3
00003498	e5933000	ldr	r3, [r3]
0000349c	e3530000	cmp	r3, #0	; 0x0
000034a0	1a000000	bne	0x34a8
000034a4	ebffffd1	bl	__ZN3dsp30protobuf_BuildDesc_dsp_2eprotoEv
000034a8	e59f3010	ldr	r3, [pc, #16]	; 0x34c0
000034ac	e08f3003	add	r3, pc, r3
000034b0	e5933000	ldr	r3, [r3]
000034b4	e1a00003	mov	r0, r3
000034b8	e8bd8080	ldmia	sp!, {r7, pc}
000034bc	00008c4c	andeq	r8, r0, ip, asr #24
000034c0	00008c34	andeq	r8, r0, r4, lsr ip
__ZN3dsp19messageType_IsValidEi:
000034c4	e92d4080	stmdb	sp!, {r7, lr}
000034c8	e28d7000	add	r7, sp, #0	; 0x0
000034cc	e24dd008	sub	sp, sp, #8	; 0x8
000034d0	e58d0004	str	r0, [sp, #4]
000034d4	e59d3004	ldr	r3, [sp, #4]
000034d8	e2433001	sub	r3, r3, #1	; 0x1
000034dc	e3530006	cmp	r3, #6	; 0x6
000034e0	8a000002	bhi	0x34f0
000034e4	e3a03001	mov	r3, #1	; 0x1
000034e8	e58d3000	str	r3, [sp]
000034ec	ea000001	b	0x34f8
000034f0	e3a03000	mov	r3, #0	; 0x0
000034f4	e58d3000	str	r3, [sp]
000034f8	e59d3000	ldr	r3, [sp]
000034fc	e1a00003	mov	r0, r3
00003500	e247d000	sub	sp, r7, #0	; 0x0
00003504	e8bd8080	ldmia	sp!, {r7, pc}
__ZN3dsp15LocationMessageC1Ev:
00003508	e92d40f0	stmdb	sp!, {r4, r5, r6, r7, lr}
0000350c	e28d700c	add	r7, sp, #12	; 0xc
00003510	e92d0d00	stmdb	sp!, {r8, r10, r11}
00003514	ed2d8b11	fstmdbx	sp!, {d8-d15}
00003518	e24dd040	sub	sp, sp, #64	; 0x40
0000351c	e58d003c	str	r0, [sp, #60]
00003520	e59f3178	ldr	r3, [pc, #376]	; 0x36a0
00003524	e08f3003	add	r3, pc, r3
00003528	e5933000	ldr	r3, [r3]
0000352c	e58d3020	str	r3, [sp, #32]
00003530	e59f316c	ldr	r3, [pc, #364]	; 0x36a4
00003534	e08f3003	add	r3, pc, r3
00003538	e58d3024	str	r3, [sp, #36]
0000353c	e28d2028	add	r2, sp, #40	; 0x28
00003540	e5827000	str	r7, [r2]
00003544	e59f315c	ldr	r3, [pc, #348]	; 0x36a8
00003548	e08f3003	add	r3, pc, r3
0000354c	e5823004	str	r3, [r2, #4]
00003550	e582d008	str	sp, [r2, #8]
00003554	e28d3008	add	r3, sp, #8	; 0x8
00003558	e1a00003	mov	r0, r3
0000355c	eb001f43	bl	0xb270	; symbol stub for: __Unwind_SjLj_Register
00003560	e59d303c	ldr	r3, [sp, #60]
00003564	e1a00003	mov	r0, r3
00003568	e59f313c	ldr	r3, [pc, #316]	; 0x36ac
0000356c	e08f3003	add	r3, pc, r3
00003570	e5933000	ldr	r3, [r3]
00003574	e12fff33	blx	r3
00003578	e59f3130	ldr	r3, [pc, #304]	; 0x36b0
0000357c	e08f3003	add	r3, pc, r3
00003580	e2832008	add	r2, r3, #8	; 0x8
00003584	e59d303c	ldr	r3, [sp, #60]
00003588	e5832000	str	r2, [r3]
0000358c	e59d303c	ldr	r3, [sp, #60]
00003590	e2832004	add	r2, r3, #4	; 0x4
00003594	e3a03001	mov	r3, #1	; 0x1
00003598	e58d300c	str	r3, [sp, #12]
0000359c	e1a00002	mov	r0, r2
000035a0	eb001f47	bl	0xb2c4	; symbol stub for: __ZN6google8protobuf15UnknownFieldSetC1Ev
000035a4	e59d203c	ldr	r2, [sp, #60]
000035a8	e3a03000	mov	r3, #0	; 0x0
000035ac	e5823008	str	r3, [r2, #8]
000035b0	e59d103c	ldr	r1, [sp, #60]
000035b4	e3a02000	mov	r2, #0	; 0x0
000035b8	e3a03000	mov	r3, #0	; 0x0
000035bc	e581200c	str	r2, [r1, #12]
000035c0	e5813010	str	r3, [r1, #16]
000035c4	e59d203c	ldr	r2, [sp, #60]
000035c8	e28f30c8	add	r3, pc, #200	; 0xc8
000035cc	e8930018	ldmia	r3, {r3, r4}
000035d0	e5823014	str	r3, [r2, #20]
000035d4	e5824018	str	r4, [r2, #24]
000035d8	e59d203c	ldr	r2, [sp, #60]
000035dc	e28f30b4	add	r3, pc, #180	; 0xb4
000035e0	e8930018	ldmia	r3, {r3, r4}
000035e4	e582301c	str	r3, [r2, #28]
000035e8	e5824020	str	r4, [r2, #32]
000035ec	e59d203c	ldr	r2, [sp, #60]
000035f0	e28f30a0	add	r3, pc, #160	; 0xa0
000035f4	e8930018	ldmia	r3, {r3, r4}
000035f8	e5823024	str	r3, [r2, #36]
000035fc	e5824028	str	r4, [r2, #40]
00003600	e59d203c	ldr	r2, [sp, #60]
00003604	e28f308c	add	r3, pc, #140	; 0x8c
00003608	e8930018	ldmia	r3, {r3, r4}
0000360c	e582302c	str	r3, [r2, #44]
00003610	e5824030	str	r4, [r2, #48]
00003614	e59d203c	ldr	r2, [sp, #60]
00003618	e28f3078	add	r3, pc, #120	; 0x78
0000361c	e8930018	ldmia	r3, {r3, r4}
00003620	e5823034	str	r3, [r2, #52]
00003624	e5824038	str	r4, [r2, #56]
00003628	e59d303c	ldr	r3, [sp, #60]
0000362c	e283203c	add	r2, r3, #60	; 0x3c
00003630	e3a03000	mov	r3, #0	; 0x0
00003634	e5823000	str	r3, [r2]
00003638	ea00000e	b	0x3678
0000363c	e59d3010	ldr	r3, [sp, #16]
00003640	e58d3000	str	r3, [sp]
00003644	e59d3000	ldr	r3, [sp]
00003648	e58d3004	str	r3, [sp, #4]
0000364c	e59d203c	ldr	r2, [sp, #60]
00003650	e3a03000	mov	r3, #0	; 0x0
00003654	e58d300c	str	r3, [sp, #12]
00003658	e1a00002	mov	r0, r2
0000365c	eb001f21	bl	0xb2e8	; symbol stub for: __ZN6google8protobuf7MessageD2Ev
00003660	e59d3004	ldr	r3, [sp, #4]
00003664	e58d3000	str	r3, [sp]
00003668	e3e03000	mvn	r3, #0	; 0x0
0000366c	e58d300c	str	r3, [sp, #12]
00003670	e59d0000	ldr	r0, [sp]
00003674	eb001f00	bl	0xb27c	; symbol stub for: __Unwind_SjLj_Resume
00003678	e28d3008	add	r3, sp, #8	; 0x8
0000367c	e1a00003	mov	r0, r3
00003680	eb001f00	bl	0xb288	; symbol stub for: __Unwind_SjLj_Unregister
00003684	e247d05c	sub	sp, r7, #92	; 0x5c
00003688	ecbd8b11	fldmiax	sp!, {d8-d15}
0000368c	e247d018	sub	sp, r7, #24	; 0x18
00003690	e8bd0d00	ldmia	sp!, {r8, r10, r11}
00003694	e8bd80f0	ldmia	sp!, {r4, r5, r6, r7, pc}
00003698	00000000	andeq	r0, r0, r0
0000369c	00000000	andeq	r0, r0, r0
000036a0	00008af4	streqd	r8, [r0], -r4
000036a4	00008fd4	ldreqd	r8, [r0], -r4
000036a8	000000ec	andeq	r0, r0, ip, ror #1
000036ac	00008ab4	streqh	r8, [r0], -r4
000036b0	00008eec	andeq	r8, r0, ip, ror #29
__ZN3dsp19LocationDescriptionC1Ev:
000036b4	e92d40f0	stmdb	sp!, {r4, r5, r6, r7, lr}
000036b8	e28d700c	add	r7, sp, #12	; 0xc
000036bc	e92d0d00	stmdb	sp!, {r8, r10, r11}
000036c0	ed2d8b11	fstmdbx	sp!, {d8-d15}
000036c4	e24dd040	sub	sp, sp, #64	; 0x40
000036c8	e58d003c	str	r0, [sp, #60]
000036cc	e59f3104	ldr	r3, [pc, #260]	; 0x37d8
000036d0	e08f3003	add	r3, pc, r3
000036d4	e5933000	ldr	r3, [r3]
000036d8	e58d3020	str	r3, [sp, #32]
000036dc	e59f30f8	ldr	r3, [pc, #248]	; 0x37dc
000036e0	e08f3003	add	r3, pc, r3
000036e4	e58d3024	str	r3, [sp, #36]
000036e8	e28d2028	add	r2, sp, #40	; 0x28
000036ec	e5827000	str	r7, [r2]
000036f0	e59f30e8	ldr	r3, [pc, #232]	; 0x37e0
000036f4	e08f3003	add	r3, pc, r3
000036f8	e5823004	str	r3, [r2, #4]
000036fc	e582d008	str	sp, [r2, #8]
00003700	e28d3008	add	r3, sp, #8	; 0x8
00003704	e1a00003	mov	r0, r3
00003708	eb001ed8	bl	0xb270	; symbol stub for: __Unwind_SjLj_Register
0000370c	e59d303c	ldr	r3, [sp, #60]
00003710	e1a00003	mov	r0, r3
00003714	e59f30c8	ldr	r3, [pc, #200]	; 0x37e4
00003718	e08f3003	add	r3, pc, r3
0000371c	e5933000	ldr	r3, [r3]
00003720	e12fff33	blx	r3
00003724	e59f30bc	ldr	r3, [pc, #188]	; 0x37e8
00003728	e08f3003	add	r3, pc, r3
0000372c	e2832008	add	r2, r3, #8	; 0x8
00003730	e59d303c	ldr	r3, [sp, #60]
00003734	e5832000	str	r2, [r3]
00003738	e59d303c	ldr	r3, [sp, #60]
0000373c	e2832004	add	r2, r3, #4	; 0x4
00003740	e3a03001	mov	r3, #1	; 0x1
00003744	e58d300c	str	r3, [sp, #12]
00003748	e1a00002	mov	r0, r2
0000374c	eb001edc	bl	0xb2c4	; symbol stub for: __ZN6google8protobuf15UnknownFieldSetC1Ev
00003750	e59d203c	ldr	r2, [sp, #60]
00003754	e3a03000	mov	r3, #0	; 0x0
00003758	e5823008	str	r3, [r2, #8]
0000375c	e59d203c	ldr	r2, [sp, #60]
00003760	e59f3084	ldr	r3, [pc, #132]	; 0x37ec
00003764	e582300c	str	r3, [r2, #12]
00003768	e59d303c	ldr	r3, [sp, #60]
0000376c	e2832010	add	r2, r3, #16	; 0x10
00003770	e3a03000	mov	r3, #0	; 0x0
00003774	e5823000	str	r3, [r2]
00003778	ea00000e	b	0x37b8
0000377c	e59d3010	ldr	r3, [sp, #16]
00003780	e58d3000	str	r3, [sp]
00003784	e59d3000	ldr	r3, [sp]
00003788	e58d3004	str	r3, [sp, #4]
0000378c	e59d203c	ldr	r2, [sp, #60]
00003790	e3a03000	mov	r3, #0	; 0x0
00003794	e58d300c	str	r3, [sp, #12]
00003798	e1a00002	mov	r0, r2
0000379c	eb001ed1	bl	0xb2e8	; symbol stub for: __ZN6google8protobuf7MessageD2Ev
000037a0	e59d3004	ldr	r3, [sp, #4]
000037a4	e58d3000	str	r3, [sp]
000037a8	e3e03000	mvn	r3, #0	; 0x0
000037ac	e58d300c	str	r3, [sp, #12]
000037b0	e59d0000	ldr	r0, [sp]
000037b4	eb001eb0	bl	0xb27c	; symbol stub for: __Unwind_SjLj_Resume
000037b8	e28d3008	add	r3, sp, #8	; 0x8
000037bc	e1a00003	mov	r0, r3
000037c0	eb001eb0	bl	0xb288	; symbol stub for: __Unwind_SjLj_Unregister
000037c4	e247d05c	sub	sp, r7, #92	; 0x5c
000037c8	ecbd8b11	fldmiax	sp!, {d8-d15}
000037cc	e247d018	sub	sp, r7, #24	; 0x18
000037d0	e8bd0d00	ldmia	sp!, {r8, r10, r11}
000037d4	e8bd80f0	ldmia	sp!, {r4, r5, r6, r7, pc}
000037d8	00008948	andeq	r8, r0, r8, asr #18
000037dc	00008e2e	andeq	r8, r0, lr, lsr #28
000037e0	00000080	andeq	r0, r0, r0, lsl #1
000037e4	00008908	andeq	r8, r0, r8, lsl #18
000037e8	00008cf8	streqd	r8, [r0], -r8
000037ec	00000000	andeq	r0, r0, r0
__ZN3dsp17MicrophoneMessageC1Ev:
000037f0	e92d40f0	stmdb	sp!, {r4, r5, r6, r7, lr}
000037f4	e28d700c	add	r7, sp, #12	; 0xc
000037f8	e92d0d00	stmdb	sp!, {r8, r10, r11}
000037fc	ed2d8b11	fstmdbx	sp!, {d8-d15}
00003800	e24dd040	sub	sp, sp, #64	; 0x40
00003804	e58d003c	str	r0, [sp, #60]
00003808	e59f311c	ldr	r3, [pc, #284]	; 0x392c
0000380c	e08f3003	add	r3, pc, r3
00003810	e5933000	ldr	r3, [r3]
00003814	e58d3020	str	r3, [sp, #32]
00003818	e59f3110	ldr	r3, [pc, #272]	; 0x3930
0000381c	e08f3003	add	r3, pc, r3
00003820	e58d3024	str	r3, [sp, #36]
00003824	e28d2028	add	r2, sp, #40	; 0x28
00003828	e5827000	str	r7, [r2]
0000382c	e59f3100	ldr	r3, [pc, #256]	; 0x3934
00003830	e08f3003	add	r3, pc, r3
00003834	e5823004	str	r3, [r2, #4]
00003838	e582d008	str	sp, [r2, #8]
0000383c	e28d3008	add	r3, sp, #8	; 0x8
00003840	e1a00003	mov	r0, r3
00003844	eb001e89	bl	0xb270	; symbol stub for: __Unwind_SjLj_Register
00003848	e59d303c	ldr	r3, [sp, #60]
0000384c	e1a00003	mov	r0, r3
00003850	e59f30e0	ldr	r3, [pc, #224]	; 0x3938
00003854	e08f3003	add	r3, pc, r3
00003858	e5933000	ldr	r3, [r3]
0000385c	e12fff33	blx	r3
00003860	e59f30d4	ldr	r3, [pc, #212]	; 0x393c
00003864	e08f3003	add	r3, pc, r3
00003868	e2832008	add	r2, r3, #8	; 0x8
0000386c	e59d303c	ldr	r3, [sp, #60]
00003870	e5832000	str	r2, [r3]
00003874	e59d303c	ldr	r3, [sp, #60]
00003878	e2832004	add	r2, r3, #4	; 0x4
0000387c	e3a03001	mov	r3, #1	; 0x1
00003880	e58d300c	str	r3, [sp, #12]
00003884	e1a00002	mov	r0, r2
00003888	eb001e8d	bl	0xb2c4	; symbol stub for: __ZN6google8protobuf15UnknownFieldSetC1Ev
0000388c	e59d203c	ldr	r2, [sp, #60]
00003890	e3a03000	mov	r3, #0	; 0x0
00003894	e5823008	str	r3, [r2, #8]
00003898	e59d103c	ldr	r1, [sp, #60]
0000389c	e3a02000	mov	r2, #0	; 0x0
000038a0	e3a03000	mov	r3, #0	; 0x0
000038a4	e581200c	str	r2, [r1, #12]
000038a8	e5813010	str	r3, [r1, #16]
000038ac	e59d203c	ldr	r2, [sp, #60]
000038b0	e59f3088	ldr	r3, [pc, #136]	; 0x3940
000038b4	e08f3003	add	r3, pc, r3
000038b8	e5823014	str	r3, [r2, #20]
000038bc	e59d303c	ldr	r3, [sp, #60]
000038c0	e2832018	add	r2, r3, #24	; 0x18
000038c4	e3a03000	mov	r3, #0	; 0x0
000038c8	e5823000	str	r3, [r2]
000038cc	ea00000e	b	0x390c
000038d0	e59d3010	ldr	r3, [sp, #16]
000038d4	e58d3000	str	r3, [sp]
000038d8	e59d3000	ldr	r3, [sp]
000038dc	e58d3004	str	r3, [sp, #4]
000038e0	e59d203c	ldr	r2, [sp, #60]
000038e4	e3a03000	mov	r3, #0	; 0x0
000038e8	e58d300c	str	r3, [sp, #12]
000038ec	e1a00002	mov	r0, r2
000038f0	eb001e7c	bl	0xb2e8	; symbol stub for: __ZN6google8protobuf7MessageD2Ev
000038f4	e59d3004	ldr	r3, [sp, #4]
000038f8	e58d3000	str	r3, [sp]
000038fc	e3e03000	mvn	r3, #0	; 0x0
00003900	e58d300c	str	r3, [sp, #12]
00003904	e59d0000	ldr	r0, [sp]
00003908	eb001e5b	bl	0xb27c	; symbol stub for: __Unwind_SjLj_Resume
0000390c	e28d3008	add	r3, sp, #8	; 0x8
00003910	e1a00003	mov	r0, r3
00003914	eb001e5b	bl	0xb288	; symbol stub for: __Unwind_SjLj_Unregister
00003918	e247d05c	sub	sp, r7, #92	; 0x5c
0000391c	ecbd8b11	fldmiax	sp!, {d8-d15}
00003920	e247d018	sub	sp, r7, #24	; 0x18
00003924	e8bd0d00	ldmia	sp!, {r8, r10, r11}
00003928	e8bd80f0	ldmia	sp!, {r4, r5, r6, r7, pc}
0000392c	0000880c	andeq	r8, r0, ip, lsl #16
00003930	00008cf8	streqd	r8, [r0], -r8
00003934	00000098	muleq	r0, r8, r0
00003938	000087cc	andeq	r8, r0, ip, asr #15
0000393c	00008b74	andeq	r8, r0, r4, ror r11
00003940	00008e24	andeq	r8, r0, r4, lsr #28
__ZN3dsp21MicrophoneDescriptionC1Ev:
00003944	e92d40f0	stmdb	sp!, {r4, r5, r6, r7, lr}
00003948	e28d700c	add	r7, sp, #12	; 0xc
0000394c	e92d0d00	stmdb	sp!, {r8, r10, r11}
00003950	ed2d8b11	fstmdbx	sp!, {d8-d15}
00003954	e24dd040	sub	sp, sp, #64	; 0x40
00003958	e58d003c	str	r0, [sp, #60]
0000395c	e59f311c	ldr	r3, [pc, #284]	; 0x3a80
00003960	e08f3003	add	r3, pc, r3
00003964	e5933000	ldr	r3, [r3]
00003968	e58d3020	str	r3, [sp, #32]
0000396c	e59f3110	ldr	r3, [pc, #272]	; 0x3a84
00003970	e08f3003	add	r3, pc, r3
00003974	e58d3024	str	r3, [sp, #36]
00003978	e28d2028	add	r2, sp, #40	; 0x28
0000397c	e5827000	str	r7, [r2]
00003980	e59f3100	ldr	r3, [pc, #256]	; 0x3a88
00003984	e08f3003	add	r3, pc, r3
00003988	e5823004	str	r3, [r2, #4]
0000398c	e582d008	str	sp, [r2, #8]
00003990	e28d3008	add	r3, sp, #8	; 0x8
00003994	e1a00003	mov	r0, r3
00003998	eb001e34	bl	0xb270	; symbol stub for: __Unwind_SjLj_Register
0000399c	e59d303c	ldr	r3, [sp, #60]
000039a0	e1a00003	mov	r0, r3
000039a4	e59f30e0	ldr	r3, [pc, #224]	; 0x3a8c
000039a8	e08f3003	add	r3, pc, r3
000039ac	e5933000	ldr	r3, [r3]
000039b0	e12fff33	blx	r3
000039b4	e59f30d4	ldr	r3, [pc, #212]	; 0x3a90
000039b8	e08f3003	add	r3, pc, r3
000039bc	e2832008	add	r2, r3, #8	; 0x8
000039c0	e59d303c	ldr	r3, [sp, #60]
000039c4	e5832000	str	r2, [r3]
000039c8	e59d303c	ldr	r3, [sp, #60]
000039cc	e2832004	add	r2, r3, #4	; 0x4
000039d0	e3a03001	mov	r3, #1	; 0x1
000039d4	e58d300c	str	r3, [sp, #12]
000039d8	e1a00002	mov	r0, r2
000039dc	eb001e38	bl	0xb2c4	; symbol stub for: __ZN6google8protobuf15UnknownFieldSetC1Ev
000039e0	e59d203c	ldr	r2, [sp, #60]
000039e4	e3a03000	mov	r3, #0	; 0x0
000039e8	e5823008	str	r3, [r2, #8]
000039ec	e59d203c	ldr	r2, [sp, #60]
000039f0	e59f309c	ldr	r3, [pc, #156]	; 0x3a94
000039f4	e582300c	str	r3, [r2, #12]
000039f8	e59d203c	ldr	r2, [sp, #60]
000039fc	e3a03000	mov	r3, #0	; 0x0
00003a00	e5823010	str	r3, [r2, #16]
00003a04	e59d203c	ldr	r2, [sp, #60]
00003a08	e3a03000	mov	r3, #0	; 0x0
00003a0c	e5823014	str	r3, [r2, #20]
00003a10	e59d303c	ldr	r3, [sp, #60]
00003a14	e2832018	add	r2, r3, #24	; 0x18
00003a18	e3a03000	mov	r3, #0	; 0x0
00003a1c	e5823000	str	r3, [r2]
00003a20	ea00000e	b	0x3a60
00003a24	e59d3010	ldr	r3, [sp, #16]
00003a28	e58d3000	str	r3, [sp]
00003a2c	e59d3000	ldr	r3, [sp]
00003a30	e58d3004	str	r3, [sp, #4]
00003a34	e59d203c	ldr	r2, [sp, #60]
00003a38	e3a03000	mov	r3, #0	; 0x0
00003a3c	e58d300c	str	r3, [sp, #12]
00003a40	e1a00002	mov	r0, r2
00003a44	eb001e27	bl	0xb2e8	; symbol stub for: __ZN6google8protobuf7MessageD2Ev
00003a48	e59d3004	ldr	r3, [sp, #4]
00003a4c	e58d3000	str	r3, [sp]
00003a50	e3e03000	mvn	r3, #0	; 0x0
00003a54	e58d300c	str	r3, [sp, #12]
00003a58	e59d0000	ldr	r0, [sp]
00003a5c	eb001e06	bl	0xb27c	; symbol stub for: __Unwind_SjLj_Resume
00003a60	e28d3008	add	r3, sp, #8	; 0x8
00003a64	e1a00003	mov	r0, r3
00003a68	eb001e06	bl	0xb288	; symbol stub for: __Unwind_SjLj_Unregister
00003a6c	e247d05c	sub	sp, r7, #92	; 0x5c
00003a70	ecbd8b11	fldmiax	sp!, {d8-d15}
00003a74	e247d018	sub	sp, r7, #24	; 0x18
00003a78	e8bd0d00	ldmia	sp!, {r8, r10, r11}
00003a7c	e8bd80f0	ldmia	sp!, {r4, r5, r6, r7, pc}
00003a80	000086b8	streqh	r8, [r0], -r8
00003a84	00008baa	andeq	r8, r0, r10, lsr #23
00003a88	00000098	muleq	r0, r8, r0
00003a8c	00008678	andeq	r8, r0, r8, ror r6
00003a90	000089d8	ldreqd	r8, [r0], -r8
00003a94	00000000	andeq	r0, r0, r0
__ZN3dsp20AccelerometerMessageC1Ev:
00003a98	e92d40f0	stmdb	sp!, {r4, r5, r6, r7, lr}
00003a9c	e28d700c	add	r7, sp, #12	; 0xc
00003aa0	e92d0d00	stmdb	sp!, {r8, r10, r11}
00003aa4	ed2d8b11	fstmdbx	sp!, {d8-d15}
00003aa8	e24dd040	sub	sp, sp, #64	; 0x40
00003aac	e58d003c	str	r0, [sp, #60]
00003ab0	e59f3130	ldr	r3, [pc, #304]	; 0x3be8
00003ab4	e08f3003	add	r3, pc, r3
00003ab8	e5933000	ldr	r3, [r3]
00003abc	e58d3020	str	r3, [sp, #32]
00003ac0	e59f3124	ldr	r3, [pc, #292]	; 0x3bec
00003ac4	e08f3003	add	r3, pc, r3
00003ac8	e58d3024	str	r3, [sp, #36]
00003acc	e28d2028	add	r2, sp, #40	; 0x28
00003ad0	e5827000	str	r7, [r2]
00003ad4	e59f3114	ldr	r3, [pc, #276]	; 0x3bf0
00003ad8	e08f3003	add	r3, pc, r3
00003adc	e5823004	str	r3, [r2, #4]
00003ae0	e582d008	str	sp, [r2, #8]
00003ae4	e28d3008	add	r3, sp, #8	; 0x8
00003ae8	e1a00003	mov	r0, r3
00003aec	eb001ddf	bl	0xb270	; symbol stub for: __Unwind_SjLj_Register
00003af0	e59d303c	ldr	r3, [sp, #60]
00003af4	e1a00003	mov	r0, r3
00003af8	e59f30f4	ldr	r3, [pc, #244]	; 0x3bf4
00003afc	e08f3003	add	r3, pc, r3
00003b00	e5933000	ldr	r3, [r3]
00003b04	e12fff33	blx	r3
00003b08	e59f30e8	ldr	r3, [pc, #232]	; 0x3bf8
00003b0c	e08f3003	add	r3, pc, r3
00003b10	e2832008	add	r2, r3, #8	; 0x8
00003b14	e59d303c	ldr	r3, [sp, #60]
00003b18	e5832000	str	r2, [r3]
00003b1c	e59d303c	ldr	r3, [sp, #60]
00003b20	e2832004	add	r2, r3, #4	; 0x4
00003b24	e3a03001	mov	r3, #1	; 0x1
00003b28	e58d300c	str	r3, [sp, #12]
00003b2c	e1a00002	mov	r0, r2
00003b30	eb001de3	bl	0xb2c4	; symbol stub for: __ZN6google8protobuf15UnknownFieldSetC1Ev
00003b34	e59d203c	ldr	r2, [sp, #60]
00003b38	e3a03000	mov	r3, #0	; 0x0
00003b3c	e5823008	str	r3, [r2, #8]
00003b40	e59d103c	ldr	r1, [sp, #60]
00003b44	e3a02000	mov	r2, #0	; 0x0
00003b48	e3a03000	mov	r3, #0	; 0x0
00003b4c	e581200c	str	r2, [r1, #12]
00003b50	e5813010	str	r3, [r1, #16]
00003b54	e59d203c	ldr	r2, [sp, #60]
00003b58	e59f309c	ldr	r3, [pc, #156]	; 0x3bfc
00003b5c	e5823014	str	r3, [r2, #20]
00003b60	e59d203c	ldr	r2, [sp, #60]
00003b64	e59f3090	ldr	r3, [pc, #144]	; 0x3bfc
00003b68	e5823018	str	r3, [r2, #24]
00003b6c	e59d203c	ldr	r2, [sp, #60]
00003b70	e59f3084	ldr	r3, [pc, #132]	; 0x3bfc
00003b74	e582301c	str	r3, [r2, #28]
00003b78	e59d303c	ldr	r3, [sp, #60]
00003b7c	e2832020	add	r2, r3, #32	; 0x20
00003b80	e3a03000	mov	r3, #0	; 0x0
00003b84	e5823000	str	r3, [r2]
00003b88	ea00000e	b	0x3bc8
00003b8c	e59d3010	ldr	r3, [sp, #16]
00003b90	e58d3000	str	r3, [sp]
00003b94	e59d3000	ldr	r3, [sp]
00003b98	e58d3004	str	r3, [sp, #4]
00003b9c	e59d203c	ldr	r2, [sp, #60]
00003ba0	e3a03000	mov	r3, #0	; 0x0
00003ba4	e58d300c	str	r3, [sp, #12]
00003ba8	e1a00002	mov	r0, r2
00003bac	eb001dcd	bl	0xb2e8	; symbol stub for: __ZN6google8protobuf7MessageD2Ev
00003bb0	e59d3004	ldr	r3, [sp, #4]
00003bb4	e58d3000	str	r3, [sp]
00003bb8	e3e03000	mvn	r3, #0	; 0x0
00003bbc	e58d300c	str	r3, [sp, #12]
00003bc0	e59d0000	ldr	r0, [sp]
00003bc4	eb001dac	bl	0xb27c	; symbol stub for: __Unwind_SjLj_Resume
00003bc8	e28d3008	add	r3, sp, #8	; 0x8
00003bcc	e1a00003	mov	r0, r3
00003bd0	eb001dac	bl	0xb288	; symbol stub for: __Unwind_SjLj_Unregister
00003bd4	e247d05c	sub	sp, r7, #92	; 0x5c
00003bd8	ecbd8b11	fldmiax	sp!, {d8-d15}
00003bdc	e247d018	sub	sp, r7, #24	; 0x18
00003be0	e8bd0d00	ldmia	sp!, {r8, r10, r11}
00003be4	e8bd80f0	ldmia	sp!, {r4, r5, r6, r7, pc}
00003be8	00008564	andeq	r8, r0, r4, ror #10
00003bec	00008a5c	andeq	r8, r0, ip, asr r10
00003bf0	000000ac	andeq	r0, r0, ip, lsr #1
00003bf4	00008524	andeq	r8, r0, r4, lsr #10
00003bf8	0000883c	andeq	r8, r0, ip, lsr r8
00003bfc	00000000	andeq	r0, r0, r0
__ZN3dsp24AccelerometerDescriptionC1Ev:
00003c00	e92d40f0	stmdb	sp!, {r4, r5, r6, r7, lr}
00003c04	e28d700c	add	r7, sp, #12	; 0xc
00003c08	e92d0d00	stmdb	sp!, {r8, r10, r11}
00003c0c	ed2d8b11	fstmdbx	sp!, {d8-d15}
00003c10	e24dd040	sub	sp, sp, #64	; 0x40
00003c14	e58d003c	str	r0, [sp, #60]
00003c18	e59f3104	ldr	r3, [pc, #260]	; 0x3d24
00003c1c	e08f3003	add	r3, pc, r3
00003c20	e5933000	ldr	r3, [r3]
00003c24	e58d3020	str	r3, [sp, #32]
00003c28	e59f30f8	ldr	r3, [pc, #248]	; 0x3d28
00003c2c	e08f3003	add	r3, pc, r3
00003c30	e58d3024	str	r3, [sp, #36]
00003c34	e28d2028	add	r2, sp, #40	; 0x28
00003c38	e5827000	str	r7, [r2]
00003c3c	e59f30e8	ldr	r3, [pc, #232]	; 0x3d2c
00003c40	e08f3003	add	r3, pc, r3
00003c44	e5823004	str	r3, [r2, #4]
00003c48	e582d008	str	sp, [r2, #8]
00003c4c	e28d3008	add	r3, sp, #8	; 0x8
00003c50	e1a00003	mov	r0, r3
00003c54	eb001d85	bl	0xb270	; symbol stub for: __Unwind_SjLj_Register
00003c58	e59d303c	ldr	r3, [sp, #60]
00003c5c	e1a00003	mov	r0, r3
00003c60	e59f30c8	ldr	r3, [pc, #200]	; 0x3d30
00003c64	e08f3003	add	r3, pc, r3
00003c68	e5933000	ldr	r3, [r3]
00003c6c	e12fff33	blx	r3
00003c70	e59f30bc	ldr	r3, [pc, #188]	; 0x3d34
00003c74	e08f3003	add	r3, pc, r3
00003c78	e2832008	add	r2, r3, #8	; 0x8
00003c7c	e59d303c	ldr	r3, [sp, #60]
00003c80	e5832000	str	r2, [r3]
00003c84	e59d303c	ldr	r3, [sp, #60]
00003c88	e2832004	add	r2, r3, #4	; 0x4
00003c8c	e3a03001	mov	r3, #1	; 0x1
00003c90	e58d300c	str	r3, [sp, #12]
00003c94	e1a00002	mov	r0, r2
00003c98	eb001d89	bl	0xb2c4	; symbol stub for: __ZN6google8protobuf15UnknownFieldSetC1Ev
00003c9c	e59d203c	ldr	r2, [sp, #60]
00003ca0	e3a03000	mov	r3, #0	; 0x0
00003ca4	e5823008	str	r3, [r2, #8]
00003ca8	e59d203c	ldr	r2, [sp, #60]
00003cac	e59f3084	ldr	r3, [pc, #132]	; 0x3d38
00003cb0	e582300c	str	r3, [r2, #12]
00003cb4	e59d303c	ldr	r3, [sp, #60]
00003cb8	e2832010	add	r2, r3, #16	; 0x10
00003cbc	e3a03000	mov	r3, #0	; 0x0
00003cc0	e5823000	str	r3, [r2]
00003cc4	ea00000e	b	0x3d04
00003cc8	e59d3010	ldr	r3, [sp, #16]
00003ccc	e58d3000	str	r3, [sp]
00003cd0	e59d3000	ldr	r3, [sp]
00003cd4	e58d3004	str	r3, [sp, #4]
00003cd8	e59d203c	ldr	r2, [sp, #60]
00003cdc	e3a03000	mov	r3, #0	; 0x0
00003ce0	e58d300c	str	r3, [sp, #12]
00003ce4	e1a00002	mov	r0, r2
00003ce8	eb001d7e	bl	0xb2e8	; symbol stub for: __ZN6google8protobuf7MessageD2Ev
00003cec	e59d3004	ldr	r3, [sp, #4]
00003cf0	e58d3000	str	r3, [sp]
00003cf4	e3e03000	mvn	r3, #0	; 0x0
00003cf8	e58d300c	str	r3, [sp, #12]
00003cfc	e59d0000	ldr	r0, [sp]
00003d00	eb001d5d	bl	0xb27c	; symbol stub for: __Unwind_SjLj_Resume
00003d04	e28d3008	add	r3, sp, #8	; 0x8
00003d08	e1a00003	mov	r0, r3
00003d0c	eb001d5d	bl	0xb288	; symbol stub for: __Unwind_SjLj_Unregister
00003d10	e247d05c	sub	sp, r7, #92	; 0x5c
00003d14	ecbd8b11	fldmiax	sp!, {d8-d15}
00003d18	e247d018	sub	sp, r7, #24	; 0x18
00003d1c	e8bd0d00	ldmia	sp!, {r8, r10, r11}
00003d20	e8bd80f0	ldmia	sp!, {r4, r5, r6, r7, pc}
00003d24	000083fc	streqd	r8, [r0], -ip
00003d28	000088fa	streqd	r8, [r0], -r10
00003d2c	00000080	andeq	r0, r0, r0, lsl #1
00003d30	000083bc	streqh	r8, [r0], -ip
00003d34	0000868c	andeq	r8, r0, ip, lsl #13
00003d38	00000000	andeq	r0, r0, r0
__ZN3dsp6HeaderC1Ev:
00003d3c	e92d40f0	stmdb	sp!, {r4, r5, r6, r7, lr}
00003d40	e28d700c	add	r7, sp, #12	; 0xc
00003d44	e92d0d00	stmdb	sp!, {r8, r10, r11}
00003d48	ed2d8b11	fstmdbx	sp!, {d8-d15}
00003d4c	e24dd040	sub	sp, sp, #64	; 0x40
00003d50	e58d003c	str	r0, [sp, #60]
00003d54	e59f3134	ldr	r3, [pc, #308]	; 0x3e90
00003d58	e08f3003	add	r3, pc, r3
00003d5c	e5933000	ldr	r3, [r3]
00003d60	e58d3020	str	r3, [sp, #32]
00003d64	e59f3128	ldr	r3, [pc, #296]	; 0x3e94
00003d68	e08f3003	add	r3, pc, r3
00003d6c	e58d3024	str	r3, [sp, #36]
00003d70	e28d2028	add	r2, sp, #40	; 0x28
00003d74	e5827000	str	r7, [r2]
00003d78	e59f3118	ldr	r3, [pc, #280]	; 0x3e98
00003d7c	e08f3003	add	r3, pc, r3
00003d80	e5823004	str	r3, [r2, #4]
00003d84	e582d008	str	sp, [r2, #8]
00003d88	e28d3008	add	r3, sp, #8	; 0x8
00003d8c	e1a00003	mov	r0, r3
00003d90	eb001d36	bl	0xb270	; symbol stub for: __Unwind_SjLj_Register
00003d94	e59d303c	ldr	r3, [sp, #60]
00003d98	e1a00003	mov	r0, r3
00003d9c	e59f30f8	ldr	r3, [pc, #248]	; 0x3e9c
00003da0	e08f3003	add	r3, pc, r3
00003da4	e5933000	ldr	r3, [r3]
00003da8	e12fff33	blx	r3
00003dac	e59f30ec	ldr	r3, [pc, #236]	; 0x3ea0
00003db0	e08f3003	add	r3, pc, r3
00003db4	e2832008	add	r2, r3, #8	; 0x8
00003db8	e59d303c	ldr	r3, [sp, #60]
00003dbc	e5832000	str	r2, [r3]
00003dc0	e59d303c	ldr	r3, [sp, #60]
00003dc4	e2832004	add	r2, r3, #4	; 0x4
00003dc8	e3a03001	mov	r3, #1	; 0x1
00003dcc	e58d300c	str	r3, [sp, #12]
00003dd0	e1a00002	mov	r0, r2
00003dd4	eb001d3a	bl	0xb2c4	; symbol stub for: __ZN6google8protobuf15UnknownFieldSetC1Ev
00003dd8	e59d203c	ldr	r2, [sp, #60]
00003ddc	e3a03000	mov	r3, #0	; 0x0
00003de0	e5823008	str	r3, [r2, #8]
00003de4	e59d203c	ldr	r2, [sp, #60]
00003de8	e59f30b4	ldr	r3, [pc, #180]	; 0x3ea4
00003dec	e08f3003	add	r3, pc, r3
00003df0	e582300c	str	r3, [r2, #12]
00003df4	e59d203c	ldr	r2, [sp, #60]
00003df8	e3a03000	mov	r3, #0	; 0x0
00003dfc	e5823010	str	r3, [r2, #16]
00003e00	e59d203c	ldr	r2, [sp, #60]
00003e04	e3a03000	mov	r3, #0	; 0x0
00003e08	e5823014	str	r3, [r2, #20]
00003e0c	e59d103c	ldr	r1, [sp, #60]
00003e10	e3a02000	mov	r2, #0	; 0x0
00003e14	e3a03000	mov	r3, #0	; 0x0
00003e18	e5812018	str	r2, [r1, #24]
00003e1c	e581301c	str	r3, [r1, #28]
00003e20	e59d303c	ldr	r3, [sp, #60]
00003e24	e2832020	add	r2, r3, #32	; 0x20
00003e28	e3a03000	mov	r3, #0	; 0x0
00003e2c	e5823000	str	r3, [r2]
00003e30	ea00000e	b	0x3e70
00003e34	e59d3010	ldr	r3, [sp, #16]
00003e38	e58d3000	str	r3, [sp]
00003e3c	e59d3000	ldr	r3, [sp]
00003e40	e58d3004	str	r3, [sp, #4]
00003e44	e59d203c	ldr	r2, [sp, #60]
00003e48	e3a03000	mov	r3, #0	; 0x0
00003e4c	e58d300c	str	r3, [sp, #12]
00003e50	e1a00002	mov	r0, r2
00003e54	eb001d23	bl	0xb2e8	; symbol stub for: __ZN6google8protobuf7MessageD2Ev
00003e58	e59d3004	ldr	r3, [sp, #4]
00003e5c	e58d3000	str	r3, [sp]
00003e60	e3e03000	mvn	r3, #0	; 0x0
00003e64	e58d300c	str	r3, [sp, #12]
00003e68	e59d0000	ldr	r0, [sp]
00003e6c	eb001d02	bl	0xb27c	; symbol stub for: __Unwind_SjLj_Resume
00003e70	e28d3008	add	r3, sp, #8	; 0x8
00003e74	e1a00003	mov	r0, r3
00003e78	eb001d02	bl	0xb288	; symbol stub for: __Unwind_SjLj_Unregister
00003e7c	e247d05c	sub	sp, r7, #92	; 0x5c
00003e80	ecbd8b11	fldmiax	sp!, {d8-d15}
00003e84	e247d018	sub	sp, r7, #24	; 0x18
00003e88	e8bd0d00	ldmia	sp!, {r8, r10, r11}
00003e8c	e8bd80f0	ldmia	sp!, {r4, r5, r6, r7, pc}
00003e90	000082c0	andeq	r8, r0, r0, asr #5
00003e94	000087c4	andeq	r8, r0, r4, asr #15
00003e98	000000b0	streqh	r0, [r0], -r0
00003e9c	00008280	andeq	r8, r0, r0, lsl #5
00003ea0	00008508	andeq	r8, r0, r8, lsl #10
00003ea4	000088f0	streqd	r8, [r0], -r0
__ZN3dsp54protobuf_BuildDesc_dsp_2eproto_AssignGlobalDescriptorsEPKN6google8protobuf14FileDescriptorE:
00003ea8	e92d40f0	stmdb	sp!, {r4, r5, r6, r7, lr}
00003eac	e28d700c	add	r7, sp, #12	; 0xc
00003eb0	e92d0d00	stmdb	sp!, {r8, r10, r11}
00003eb4	ed2d8b11	fstmdbx	sp!, {d8-d15}
00003eb8	e24dd0e0	sub	sp, sp, #224	; 0xe0
00003ebc	e58d00dc	str	r0, [sp, #220]
00003ec0	e59f3c5c	ldr	r3, [pc, #3164]	; 0x4b24
00003ec4	e08f3003	add	r3, pc, r3
00003ec8	e5933000	ldr	r3, [r3]
00003ecc	e58d30c0	str	r3, [sp, #192]
00003ed0	e59f3c50	ldr	r3, [pc, #3152]	; 0x4b28
00003ed4	e08f3003	add	r3, pc, r3
00003ed8	e58d30c4	str	r3, [sp, #196]
00003edc	e28d20c8	add	r2, sp, #200	; 0xc8
00003ee0	e5827000	str	r7, [r2]
00003ee4	e59f3c40	ldr	r3, [pc, #3136]	; 0x4b2c
00003ee8	e08f3003	add	r3, pc, r3
00003eec	e5823004	str	r3, [r2, #4]
00003ef0	e582d008	str	sp, [r2, #8]
00003ef4	e28d30a8	add	r3, sp, #168	; 0xa8
00003ef8	e1a00003	mov	r0, r3
00003efc	eb001cdb	bl	0xb270	; symbol stub for: __Unwind_SjLj_Register
00003f00	e59d00dc	ldr	r0, [sp, #220]
00003f04	e3a01000	mov	r1, #0	; 0x0
00003f08	e59f3c20	ldr	r3, [pc, #3104]	; 0x4b30
00003f0c	e08f3003	add	r3, pc, r3
00003f10	e5933000	ldr	r3, [r3]
00003f14	e12fff33	blx	r3
00003f18	e1a02000	mov	r2, r0
00003f1c	e59f3c10	ldr	r3, [pc, #3088]	; 0x4b34
00003f20	e08f3003	add	r3, pc, r3
00003f24	e5832000	str	r2, [r3]
00003f28	e3e03000	mvn	r3, #0	; 0x0
00003f2c	e58d30ac	str	r3, [sp, #172]
00003f30	e3a00024	mov	r0, #36	; 0x24
00003f34	eb001d06	bl	0xb354	; symbol stub for: __Znwm
00003f38	e1a03000	mov	r3, r0
00003f3c	e58d301c	str	r3, [sp, #28]
00003f40	e3a0300e	mov	r3, #14	; 0xe
00003f44	e58d30ac	str	r3, [sp, #172]
00003f48	e59d001c	ldr	r0, [sp, #28]
00003f4c	ebffff7a	bl	__ZN3dsp6HeaderC1Ev
00003f50	e59f3be0	ldr	r3, [pc, #3040]	; 0x4b38
00003f54	e08f3003	add	r3, pc, r3
00003f58	e59d201c	ldr	r2, [sp, #28]
00003f5c	e5832000	str	r2, [r3]
00003f60	e3e03000	mvn	r3, #0	; 0x0
00003f64	e58d30ac	str	r3, [sp, #172]
00003f68	eb001cc9	bl	0xb294	; symbol stub for: __ZN6google8protobuf14DescriptorPool14generated_poolEv
00003f6c	e1a03000	mov	r3, r0
00003f70	e58d3020	str	r3, [sp, #32]
00003f74	e3a00024	mov	r0, #36	; 0x24
00003f78	eb001cf5	bl	0xb354	; symbol stub for: __Znwm
00003f7c	e1a03000	mov	r3, r0
00003f80	e58d3024	str	r3, [sp, #36]
00003f84	e59f3bb0	ldr	r3, [pc, #2992]	; 0x4b3c
00003f88	e08f3003	add	r3, pc, r3
00003f8c	e5931000	ldr	r1, [r3]
00003f90	e59f3ba8	ldr	r3, [pc, #2984]	; 0x4b40
00003f94	e08f3003	add	r3, pc, r3
00003f98	e5933000	ldr	r3, [r3]
00003f9c	e1a0c003	mov	ip, r3
00003fa0	e3a03010	mov	r3, #16	; 0x10
00003fa4	e2833020	add	r3, r3, #32	; 0x20
00003fa8	e2432010	sub	r2, r3, #16	; 0x10
00003fac	e3a03010	mov	r3, #16	; 0x10
00003fb0	e2833004	add	r3, r3, #4	; 0x4
00003fb4	e2433010	sub	r3, r3, #16	; 0x10
00003fb8	e58d2000	str	r2, [sp]
00003fbc	e58d3004	str	r3, [sp, #4]
00003fc0	e3e03000	mvn	r3, #0	; 0x0
00003fc4	e58d3008	str	r3, [sp, #8]
00003fc8	e59d3020	ldr	r3, [sp, #32]
00003fcc	e58d300c	str	r3, [sp, #12]
00003fd0	e3a03024	mov	r3, #36	; 0x24
00003fd4	e58d3010	str	r3, [sp, #16]
00003fd8	e3a0300d	mov	r3, #13	; 0xd
00003fdc	e58d30ac	str	r3, [sp, #172]
00003fe0	e59d0024	ldr	r0, [sp, #36]
00003fe4	e1a0200c	mov	r2, ip
00003fe8	e59f3b54	ldr	r3, [pc, #2900]	; 0x4b44
00003fec	e08f3003	add	r3, pc, r3
00003ff0	eb001cc2	bl	0xb300	; symbol stub for: __ZN6google8protobuf8internal26GeneratedMessageReflectionC1EPKNS0_10DescriptorEPKNS0_7MessageEPKiiiiPKNS0_14DescriptorPoolEi
00003ff4	ea000009	b	0x4020
00003ff8	e59d2018	ldr	r2, [sp, #24]
00003ffc	e58d2070	str	r2, [sp, #112]
00004000	e59d001c	ldr	r0, [sp, #28]
00004004	eb001ccf	bl	0xb348	; symbol stub for: __ZdlPv
00004008	e59d3070	ldr	r3, [sp, #112]
0000400c	e58d3018	str	r3, [sp, #24]
00004010	e3e03000	mvn	r3, #0	; 0x0
00004014	e58d30ac	str	r3, [sp, #172]
00004018	e59d0018	ldr	r0, [sp, #24]
0000401c	eb001c96	bl	0xb27c	; symbol stub for: __Unwind_SjLj_Resume
00004020	e59f3b20	ldr	r3, [pc, #2848]	; 0x4b48
00004024	e08f3003	add	r3, pc, r3
00004028	e59d2024	ldr	r2, [sp, #36]
0000402c	e5832000	str	r2, [r3]
00004030	e59f3b14	ldr	r3, [pc, #2836]	; 0x4b4c
00004034	e08f3003	add	r3, pc, r3
00004038	e5931000	ldr	r1, [r3]
0000403c	e59f3b0c	ldr	r3, [pc, #2828]	; 0x4b50
00004040	e08f3003	add	r3, pc, r3
00004044	e5933000	ldr	r3, [r3]
00004048	e1a02003	mov	r2, r3
0000404c	e3e03000	mvn	r3, #0	; 0x0
00004050	e58d30ac	str	r3, [sp, #172]
00004054	e1a00001	mov	r0, r1
00004058	e1a01002	mov	r1, r2
0000405c	eb001c95	bl	0xb2b8	; symbol stub for: __ZN6google8protobuf14MessageFactory32InternalRegisterGeneratedMessageEPKNS0_10DescriptorEPKNS0_7MessageE
00004060	e59d00dc	ldr	r0, [sp, #220]
00004064	e3a01001	mov	r1, #1	; 0x1
00004068	e59f3ae4	ldr	r3, [pc, #2788]	; 0x4b54
0000406c	e08f3003	add	r3, pc, r3
00004070	e5933000	ldr	r3, [r3]
00004074	e12fff33	blx	r3
00004078	e1a02000	mov	r2, r0
0000407c	e59f3ad4	ldr	r3, [pc, #2772]	; 0x4b58
00004080	e08f3003	add	r3, pc, r3
00004084	e5832000	str	r2, [r3]
00004088	e3a00014	mov	r0, #20	; 0x14
0000408c	eb001cb0	bl	0xb354	; symbol stub for: __Znwm
00004090	e1a03000	mov	r3, r0
00004094	e58d3028	str	r3, [sp, #40]
00004098	e3a0300c	mov	r3, #12	; 0xc
0000409c	e58d30ac	str	r3, [sp, #172]
000040a0	e59d0028	ldr	r0, [sp, #40]
000040a4	ebfffed5	bl	__ZN3dsp24AccelerometerDescriptionC1Ev
000040a8	ea000009	b	0x40d4
000040ac	e59d3018	ldr	r3, [sp, #24]
000040b0	e58d3074	str	r3, [sp, #116]
000040b4	e59d0024	ldr	r0, [sp, #36]
000040b8	eb001ca2	bl	0xb348	; symbol stub for: __ZdlPv
000040bc	e59d2074	ldr	r2, [sp, #116]
000040c0	e58d2018	str	r2, [sp, #24]
000040c4	e3e03000	mvn	r3, #0	; 0x0
000040c8	e58d30ac	str	r3, [sp, #172]
000040cc	e59d0018	ldr	r0, [sp, #24]
000040d0	eb001c69	bl	0xb27c	; symbol stub for: __Unwind_SjLj_Resume
000040d4	e59f3a80	ldr	r3, [pc, #2688]	; 0x4b5c
000040d8	e08f3003	add	r3, pc, r3
000040dc	e59d2028	ldr	r2, [sp, #40]
000040e0	e5832000	str	r2, [r3]
000040e4	e3e03000	mvn	r3, #0	; 0x0
000040e8	e58d30ac	str	r3, [sp, #172]
000040ec	eb001c68	bl	0xb294	; symbol stub for: __ZN6google8protobuf14DescriptorPool14generated_poolEv
000040f0	e1a03000	mov	r3, r0
000040f4	e58d302c	str	r3, [sp, #44]
000040f8	e3a00024	mov	r0, #36	; 0x24
000040fc	eb001c94	bl	0xb354	; symbol stub for: __Znwm
00004100	e1a03000	mov	r3, r0
00004104	e58d3030	str	r3, [sp, #48]
00004108	e59f3a50	ldr	r3, [pc, #2640]	; 0x4b60
0000410c	e08f3003	add	r3, pc, r3
00004110	e5931000	ldr	r1, [r3]
00004114	e59f3a48	ldr	r3, [pc, #2632]	; 0x4b64
00004118	e08f3003	add	r3, pc, r3
0000411c	e5933000	ldr	r3, [r3]
00004120	e1a0c003	mov	ip, r3
00004124	e3a03010	mov	r3, #16	; 0x10
00004128	e2833010	add	r3, r3, #16	; 0x10
0000412c	e2432010	sub	r2, r3, #16	; 0x10
00004130	e3a03010	mov	r3, #16	; 0x10
00004134	e2833004	add	r3, r3, #4	; 0x4
00004138	e2433010	sub	r3, r3, #16	; 0x10
0000413c	e58d2000	str	r2, [sp]
00004140	e58d3004	str	r3, [sp, #4]
00004144	e3e03000	mvn	r3, #0	; 0x0
00004148	e58d3008	str	r3, [sp, #8]
0000414c	e59d302c	ldr	r3, [sp, #44]
00004150	e58d300c	str	r3, [sp, #12]
00004154	e3a03014	mov	r3, #20	; 0x14
00004158	e58d3010	str	r3, [sp, #16]
0000415c	e3a0300b	mov	r3, #11	; 0xb
00004160	e58d30ac	str	r3, [sp, #172]
00004164	e59d0030	ldr	r0, [sp, #48]
00004168	e1a0200c	mov	r2, ip
0000416c	e59f39f4	ldr	r3, [pc, #2548]	; 0x4b68
00004170	e08f3003	add	r3, pc, r3
00004174	eb001c61	bl	0xb300	; symbol stub for: __ZN6google8protobuf8internal26GeneratedMessageReflectionC1EPKNS0_10DescriptorEPKNS0_7MessageEPKiiiiPKNS0_14DescriptorPoolEi
00004178	ea000009	b	0x41a4
0000417c	e59d2018	ldr	r2, [sp, #24]
00004180	e58d2078	str	r2, [sp, #120]
00004184	e59d0028	ldr	r0, [sp, #40]
00004188	eb001c6e	bl	0xb348	; symbol stub for: __ZdlPv
0000418c	e59d3078	ldr	r3, [sp, #120]
00004190	e58d3018	str	r3, [sp, #24]
00004194	e3e03000	mvn	r3, #0	; 0x0
00004198	e58d30ac	str	r3, [sp, #172]
0000419c	e59d0018	ldr	r0, [sp, #24]
000041a0	eb001c35	bl	0xb27c	; symbol stub for: __Unwind_SjLj_Resume
000041a4	e59f39c0	ldr	r3, [pc, #2496]	; 0x4b6c
000041a8	e08f3003	add	r3, pc, r3
000041ac	e59d2030	ldr	r2, [sp, #48]
000041b0	e5832000	str	r2, [r3]
000041b4	e59f39b4	ldr	r3, [pc, #2484]	; 0x4b70
000041b8	e08f3003	add	r3, pc, r3
000041bc	e5931000	ldr	r1, [r3]
000041c0	e59f39ac	ldr	r3, [pc, #2476]	; 0x4b74
000041c4	e08f3003	add	r3, pc, r3
000041c8	e5933000	ldr	r3, [r3]
000041cc	e1a02003	mov	r2, r3
000041d0	e3e03000	mvn	r3, #0	; 0x0
000041d4	e58d30ac	str	r3, [sp, #172]
000041d8	e1a00001	mov	r0, r1
000041dc	e1a01002	mov	r1, r2
000041e0	eb001c34	bl	0xb2b8	; symbol stub for: __ZN6google8protobuf14MessageFactory32InternalRegisterGeneratedMessageEPKNS0_10DescriptorEPKNS0_7MessageE
000041e4	e59d00dc	ldr	r0, [sp, #220]
000041e8	e3a01002	mov	r1, #2	; 0x2
000041ec	e59f3984	ldr	r3, [pc, #2436]	; 0x4b78
000041f0	e08f3003	add	r3, pc, r3
000041f4	e5933000	ldr	r3, [r3]
000041f8	e12fff33	blx	r3
000041fc	e1a02000	mov	r2, r0
00004200	e59f3974	ldr	r3, [pc, #2420]	; 0x4b7c
00004204	e08f3003	add	r3, pc, r3
00004208	e5832000	str	r2, [r3]
0000420c	e3a00024	mov	r0, #36	; 0x24
00004210	eb001c4f	bl	0xb354	; symbol stub for: __Znwm
00004214	e1a03000	mov	r3, r0
00004218	e58d3034	str	r3, [sp, #52]
0000421c	e3a0300a	mov	r3, #10	; 0xa
00004220	e58d30ac	str	r3, [sp, #172]
00004224	e59d0034	ldr	r0, [sp, #52]
00004228	ebfffe1a	bl	__ZN3dsp20AccelerometerMessageC1Ev
0000422c	ea000009	b	0x4258
00004230	e59d3018	ldr	r3, [sp, #24]
00004234	e58d307c	str	r3, [sp, #124]
00004238	e59d0030	ldr	r0, [sp, #48]
0000423c	eb001c41	bl	0xb348	; symbol stub for: __ZdlPv
00004240	e59d207c	ldr	r2, [sp, #124]
00004244	e58d2018	str	r2, [sp, #24]
00004248	e3e03000	mvn	r3, #0	; 0x0
0000424c	e58d30ac	str	r3, [sp, #172]
00004250	e59d0018	ldr	r0, [sp, #24]
00004254	eb001c08	bl	0xb27c	; symbol stub for: __Unwind_SjLj_Resume
00004258	e59f3920	ldr	r3, [pc, #2336]	; 0x4b80
0000425c	e08f3003	add	r3, pc, r3
00004260	e59d2034	ldr	r2, [sp, #52]
00004264	e5832000	str	r2, [r3]
00004268	e3e03000	mvn	r3, #0	; 0x0
0000426c	e58d30ac	str	r3, [sp, #172]
00004270	eb001c07	bl	0xb294	; symbol stub for: __ZN6google8protobuf14DescriptorPool14generated_poolEv
00004274	e1a03000	mov	r3, r0
00004278	e58d3038	str	r3, [sp, #56]
0000427c	e3a00024	mov	r0, #36	; 0x24
00004280	eb001c33	bl	0xb354	; symbol stub for: __Znwm
00004284	e1a03000	mov	r3, r0
00004288	e58d303c	str	r3, [sp, #60]
0000428c	e59f38f0	ldr	r3, [pc, #2288]	; 0x4b84
00004290	e08f3003	add	r3, pc, r3
00004294	e5931000	ldr	r1, [r3]
00004298	e59f38e8	ldr	r3, [pc, #2280]	; 0x4b88
0000429c	e08f3003	add	r3, pc, r3
000042a0	e5933000	ldr	r3, [r3]
000042a4	e1a0c003	mov	ip, r3
000042a8	e3a03010	mov	r3, #16	; 0x10
000042ac	e2833020	add	r3, r3, #32	; 0x20
000042b0	e2432010	sub	r2, r3, #16	; 0x10
000042b4	e3a03010	mov	r3, #16	; 0x10
000042b8	e2833004	add	r3, r3, #4	; 0x4
000042bc	e2433010	sub	r3, r3, #16	; 0x10
000042c0	e58d2000	str	r2, [sp]
000042c4	e58d3004	str	r3, [sp, #4]
000042c8	e3e03000	mvn	r3, #0	; 0x0
000042cc	e58d3008	str	r3, [sp, #8]
000042d0	e59d3038	ldr	r3, [sp, #56]
000042d4	e58d300c	str	r3, [sp, #12]
000042d8	e3a03024	mov	r3, #36	; 0x24
000042dc	e58d3010	str	r3, [sp, #16]
000042e0	e3a03009	mov	r3, #9	; 0x9
000042e4	e58d30ac	str	r3, [sp, #172]
000042e8	e59d003c	ldr	r0, [sp, #60]
000042ec	e1a0200c	mov	r2, ip
000042f0	e59f3894	ldr	r3, [pc, #2196]	; 0x4b8c
000042f4	e08f3003	add	r3, pc, r3
000042f8	eb001c00	bl	0xb300	; symbol stub for: __ZN6google8protobuf8internal26GeneratedMessageReflectionC1EPKNS0_10DescriptorEPKNS0_7MessageEPKiiiiPKNS0_14DescriptorPoolEi
000042fc	ea000009	b	0x4328
00004300	e59d2018	ldr	r2, [sp, #24]
00004304	e58d2080	str	r2, [sp, #128]
00004308	e59d0034	ldr	r0, [sp, #52]
0000430c	eb001c0d	bl	0xb348	; symbol stub for: __ZdlPv
00004310	e59d3080	ldr	r3, [sp, #128]
00004314	e58d3018	str	r3, [sp, #24]
00004318	e3e03000	mvn	r3, #0	; 0x0
0000431c	e58d30ac	str	r3, [sp, #172]
00004320	e59d0018	ldr	r0, [sp, #24]
00004324	eb001bd4	bl	0xb27c	; symbol stub for: __Unwind_SjLj_Resume
00004328	e59f3860	ldr	r3, [pc, #2144]	; 0x4b90
0000432c	e08f3003	add	r3, pc, r3
00004330	e59d203c	ldr	r2, [sp, #60]
00004334	e5832000	str	r2, [r3]
00004338	e59f3854	ldr	r3, [pc, #2132]	; 0x4b94
0000433c	e08f3003	add	r3, pc, r3
00004340	e5931000	ldr	r1, [r3]
00004344	e59f384c	ldr	r3, [pc, #2124]	; 0x4b98
00004348	e08f3003	add	r3, pc, r3
0000434c	e5933000	ldr	r3, [r3]
00004350	e1a02003	mov	r2, r3
00004354	e3e03000	mvn	r3, #0	; 0x0
00004358	e58d30ac	str	r3, [sp, #172]
0000435c	e1a00001	mov	r0, r1
00004360	e1a01002	mov	r1, r2
00004364	eb001bd3	bl	0xb2b8	; symbol stub for: __ZN6google8protobuf14MessageFactory32InternalRegisterGeneratedMessageEPKNS0_10DescriptorEPKNS0_7MessageE
00004368	e59d00dc	ldr	r0, [sp, #220]
0000436c	e3a01003	mov	r1, #3	; 0x3
00004370	e59f3824	ldr	r3, [pc, #2084]	; 0x4b9c
00004374	e08f3003	add	r3, pc, r3
00004378	e5933000	ldr	r3, [r3]
0000437c	e12fff33	blx	r3
00004380	e1a02000	mov	r2, r0
00004384	e59f3814	ldr	r3, [pc, #2068]	; 0x4ba0
00004388	e08f3003	add	r3, pc, r3
0000438c	e5832000	str	r2, [r3]
00004390	e3a0001c	mov	r0, #28	; 0x1c
00004394	eb001bee	bl	0xb354	; symbol stub for: __Znwm
00004398	e1a03000	mov	r3, r0
0000439c	e58d3040	str	r3, [sp, #64]
000043a0	e3a03008	mov	r3, #8	; 0x8
000043a4	e58d30ac	str	r3, [sp, #172]
000043a8	e59d0040	ldr	r0, [sp, #64]
000043ac	ebfffd64	bl	__ZN3dsp21MicrophoneDescriptionC1Ev
000043b0	ea000009	b	0x43dc
000043b4	e59d3018	ldr	r3, [sp, #24]
000043b8	e58d3084	str	r3, [sp, #132]
000043bc	e59d003c	ldr	r0, [sp, #60]
000043c0	eb001be0	bl	0xb348	; symbol stub for: __ZdlPv
000043c4	e59d2084	ldr	r2, [sp, #132]
000043c8	e58d2018	str	r2, [sp, #24]
000043cc	e3e03000	mvn	r3, #0	; 0x0
000043d0	e58d30ac	str	r3, [sp, #172]
000043d4	e59d0018	ldr	r0, [sp, #24]
000043d8	eb001ba7	bl	0xb27c	; symbol stub for: __Unwind_SjLj_Resume
000043dc	e59f37c0	ldr	r3, [pc, #1984]	; 0x4ba4
000043e0	e08f3003	add	r3, pc, r3
000043e4	e59d2040	ldr	r2, [sp, #64]
000043e8	e5832000	str		r2, [r3]
000043ec	e3e03000	mvn	r3, #0	; 0x0
000043f0	e58d30ac	str	r3, [sp, #172]
000043f4	eb001ba6	bl	0xb294	; symbol stub for: __ZN6google8protobuf14DescriptorPool14generated_poolEv
000043f8	e1a03000	mov	r3, r0
000043fc	e58d3044	str	r3, [sp, #68]
00004400	e3a00024	mov	r0, #36	; 0x24
00004404	eb001bd2	bl	0xb354	; symbol stub for: __Znwm
00004408	e1a03000	mov	r3, r0
0000440c	e58d3048	str	r3, [sp, #72]
00004410	e59f3790	ldr	r3, [pc, #1936]	; 0x4ba8
00004414	e08f3003	add	r3, pc, r3
00004418	e5931000	ldr	r1, [r3]
0000441c	e59f3788	ldr	r3, [pc, #1928]	; 0x4bac
00004420	e08f3003	add	r3, pc, r3
00004424	e5933000	ldr	r3, [r3]
00004428	e1a0c003	mov	ip, r3
0000442c	e3a03010	mov	r3, #16	; 0x10
00004430	e2833018	add	r3, r3, #24	; 0x18
00004434	e2432010	sub	r2, r3, #16	; 0x10
00004438	e3a03010	mov	r3, #16	; 0x10
0000443c	e2833004	add	r3, r3, #4	; 0x4
00004440	e2433010	sub	r3, r3, #16	; 0x10
00004444	e58d2000	str	r2, [sp]
00004448	e58d3004	str	r3, [sp, #4]
0000444c	e3e03000	mvn	r3, #0	; 0x0
00004450	e58d3008	str	r3, [sp, #8]
00004454	e59d3044	ldr	r3, [sp, #68]
00004458	e58d300c	str	r3, [sp, #12]
0000445c	e3a0301c	mov	r3, #28	; 0x1c
00004460	e58d3010	str	r3, [sp, #16]
00004464	e3a03007	mov	r3, #7	; 0x7
00004468	e58d30ac	str	r3, [sp, #172]
0000446c	e59d0048	ldr	r0, [sp, #72]
00004470	e1a0200c	mov	r2, ip
00004474	e59f3734	ldr	r3, [pc, #1844]	; 0x4bb0
00004478	e08f3003	add	r3, pc, r3
0000447c	eb001b9f	bl	0xb300	; symbol stub for: __ZN6google8protobuf8internal26GeneratedMessageReflectionC1EPKNS0_10DescriptorEPKNS0_7MessageEPKiiiiPKNS0_14DescriptorPoolEi
00004480	ea000009	b	0x44ac
00004484	e59d2018	ldr	r2, [sp, #24]
00004488	e58d2088	str	r2, [sp, #136]
0000448c	e59d0040	ldr	r0, [sp, #64]
00004490	eb001bac	bl	0xb348	; symbol stub for: __ZdlPv
00004494	e59d3088	ldr	r3, [sp, #136]
00004498	e58d3018	str	r3, [sp, #24]
0000449c	e3e03000	mvn	r3, #0	; 0x0
000044a0	e58d30ac	str	r3, [sp, #172]
000044a4	e59d0018	ldr	r0, [sp, #24]
000044a8	eb001b73	bl	0xb27c	; symbol stub for: __Unwind_SjLj_Resume
000044ac	e59f3700	ldr	r3, [pc, #1792]	; 0x4bb4
000044b0	e08f3003	add	r3, pc, r3
000044b4	e59d2048	ldr	r2, [sp, #72]
000044b8	e5832000	str	r2, [r3]
000044bc	e59f36f4	ldr	r3, [pc, #1780]	; 0x4bb8
000044c0	e08f3003	add	r3, pc, r3
000044c4	e5931000	ldr	r1, [r3]
000044c8	e59f36ec	ldr	r3, [pc, #1772]	; 0x4bbc
000044cc	e08f3003	add	r3, pc, r3
000044d0	e5933000	ldr	r3, [r3]
000044d4	e1a02003	mov	r2, r3
000044d8	e3e03000	mvn	r3, #0	; 0x0
000044dc	e58d30ac	str	r3, [sp, #172]
000044e0	e1a00001	mov	r0, r1
000044e4	e1a01002	mov	r1, r2
000044e8	eb001b72	bl	0xb2b8	; symbol stub for: __ZN6google8protobuf14MessageFactory32InternalRegisterGeneratedMessageEPKNS0_10DescriptorEPKNS0_7MessageE
000044ec	e59d00dc	ldr	r0, [sp, #220]
000044f0	e3a01004	mov	r1, #4	; 0x4
000044f4	e59f36c4	ldr	r3, [pc, #1732]	; 0x4bc0
000044f8	e08f3003	add	r3, pc, r3
000044fc	e5933000	ldr	r3, [r3]
00004500	e12fff33	blx	r3
00004504	e1a02000	mov	r2, r0
00004508	e59f36b4	ldr	r3, [pc, #1716]	; 0x4bc4
0000450c	e08f3003	add	r3, pc, r3
00004510	e5832000	str	r2, [r3]
00004514	e3a0001c	mov	r0, #28	; 0x1c
00004518	eb001b8d	bl	0xb354	; symbol stub for: __Znwm
0000451c	e1a03000	mov	r3, r0
00004520	e58d304c	str	r3, [sp, #76]
00004524	e3a03006	mov	r3, #6	; 0x6
00004528	e58d30ac	str	r3, [sp, #172]
0000452c	e59d004c	ldr	r0, [sp, #76]
00004530	ebfffcae	bl	__ZN3dsp17MicrophoneMessageC1Ev
00004534	ea000009	b	0x4560
00004538	e59d3018	ldr	r3, [sp, #24]
0000453c	e58d308c	str	r3, [sp, #140]
00004540	e59d0048	ldr	r0, [sp, #72]
00004544	eb001b7f	bl	0xb348	; symbol stub for: __ZdlPv
00004548	e59d208c	ldr	r2, [sp, #140]
0000454c	e58d2018	str	r2, [sp, #24]
00004550	e3e03000	mvn	r3, #0	; 0x0
00004554	e58d30ac	str	r3, [sp, #172]
00004558	e59d0018	ldr	r0, [sp, #24]
0000455c	eb001b46	bl	0xb27c	; symbol stub for: __Unwind_SjLj_Resume
00004560	e59f3660	ldr	r3, [pc, #1632]	; 0x4bc8
00004564	e08f3003	add	r3, pc, r3
00004568	e59d204c	ldr	r2, [sp, #76]
0000456c	e5832000	str	r2, [r3]
00004570	e3e03000	mvn	r3, #0	; 0x0
00004574	e58d30ac	str	r3, [sp, #172]
00004578	eb001b45	bl	0xb294	; symbol stub for: __ZN6google8protobuf14DescriptorPool14generated_poolEv
0000457c	e1a03000	mov	r3, r0
00004580	e58d3050	str	r3, [sp, #80]
00004584	e3a00024	mov	r0, #36	; 0x24
00004588	eb001b71	bl	0xb354	; symbol stub for: __Znwm
0000458c	e1a03000	mov	r3, r0
00004590	e58d3054	str	r3, [sp, #84]
00004594	e59f3630	ldr	r3, [pc, #1584]	; 0x4bcc
00004598	e08f3003	add	r3, pc, r3
0000459c	e5931000	ldr	r1, [r3]
000045a0	e59f3628	ldr	r3, [pc, #1576]	; 0x4bd0
000045a4	e08f3003	add	r3, pc, r3
000045a8	e5933000	ldr	r3, [r3]
000045ac	e1a0c003	mov	ip, r3
000045b0	e3a03010	mov	r3, #16	; 0x10
000045b4	e2833018	add	r3, r3, #24	; 0x18
000045b8	e2432010	sub	r2, r3, #16	; 0x10
000045bc	e3a03010	mov	r3, #16	; 0x10
000045c0	e2833004	add	r3, r3, #4	; 0x4
000045c4	e2433010	sub	r3, r3, #16	; 0x10
000045c8	e58d2000	str	r2, [sp]
000045cc	e58d3004	str	r3, [sp, #4]
000045d0	e3e03000	mvn	r3, #0	; 0x0
000045d4	e58d3008	str	r3, [sp, #8]
000045d8	e59d3050	ldr	r3, [sp, #80]
000045dc	e58d300c	str	r3, [sp, #12]
000045e0	e3a0301c	mov	r3, #28	; 0x1c
000045e4	e58d3010	str	r3, [sp, #16]
000045e8	e3a03005	mov	r3, #5	; 0x5
000045ec	e58d30ac	str	r3, [sp, #172]
000045f0	e59d0054	ldr	r0, [sp, #84]
000045f4	e1a0200c	mov	r2, ip
000045f8	e59f35d4	ldr	r3, [pc, #1492]	; 0x4bd4
000045fc	e08f3003	add	r3, pc, r3
00004600	eb001b3e	bl	0xb300	; symbol stub for: __ZN6google8protobuf8internal26GeneratedMessageReflectionC1EPKNS0_10DescriptorEPKNS0_7MessageEPKiiiiPKNS0_14DescriptorPoolEi
00004604	ea000009	b	0x4630
00004608	e59d2018	ldr	r2, [sp, #24]
0000460c	e58d2090	str	r2, [sp, #144]
00004610	e59d004c	ldr	r0, [sp, #76]
00004614	eb001b4b	bl	0xb348	; symbol stub for: __ZdlPv
00004618	e59d3090	ldr	r3, [sp, #144]
0000461c	e58d3018	str	r3, [sp, #24]
00004620	e3e03000	mvn	r3, #0	; 0x0
00004624	e58d30ac	str	r3, [sp, #172]
00004628	e59d0018	ldr	r0, [sp, #24]
0000462c	eb001b12	bl	0xb27c	; symbol stub for: __Unwind_SjLj_Resume
00004630	e59f35a0	ldr	r3, [pc, #1440]	; 0x4bd8
00004634	e08f3003	add	r3, pc, r3
00004638	e59d2054	ldr	r2, [sp, #84]
0000463c	e5832000	str	r2, [r3]
00004640	e59f3594	ldr	r3, [pc, #1428]	; 0x4bdc
00004644	e08f3003	add	r3, pc, r3
00004648	e5931000	ldr	r1, [r3]
0000464c	e59f358c	ldr	r3, [pc, #1420]	; 0x4be0
00004650	e08f3003	add	r3, pc, r3
00004654	e5933000	ldr	r3, [r3]
00004658	e1a02003	mov	r2, r3
0000465c	e3e03000	mvn	r3, #0	; 0x0
00004660	e58d30ac	str	r3, [sp, #172]
00004664	e1a00001	mov	r0, r1
00004668	e1a01002	mov	r1, r2
0000466c	eb001b11	bl	0xb2b8	; symbol stub for: __ZN6google8protobuf14MessageFactory32InternalRegisterGeneratedMessageEPKNS0_10DescriptorEPKNS0_7MessageE
00004670	e59d00dc	ldr	r0, [sp, #220]
00004674	e3a01005	mov	r1, #5	; 0x5
00004678	e59f3564	ldr	r3, [pc, #1380]	; 0x4be4
0000467c	e08f3003	add	r3, pc, r3
00004680	e5933000	ldr	r3, [r3]
00004684	e12fff33	blx	r3
00004688	e1a02000	mov	r2, r0
0000468c	e59f3554	ldr	r3, [pc, #1364]	; 0x4be8
00004690	e08f3003	add	r3, pc, r3
00004694	e5832000	str	r2, [r3]
00004698	e3a00014	mov	r0, #20	; 0x14
0000469c	eb001b2c	bl	0xb354	; symbol stub for: __Znwm
000046a0	e1a03000	mov	r3, r0
000046a4	e58d3058	str	r3, [sp, #88]
000046a8	e3a03004	mov	r3, #4	; 0x4
000046ac	e58d30ac	str	r3, [sp, #172]
000046b0	e59d0058	ldr	r0, [sp, #88]
000046b4	ebfffbfe	bl	__ZN3dsp19LocationDescriptionC1Ev
000046b8	ea000009	b	0x46e4
000046bc	e59d3018	ldr	r3, [sp, #24]
000046c0	e58d3094	str	r3, [sp, #148]
000046c4	e59d0054	ldr	r0, [sp, #84]
000046c8	eb001b1e	bl	0xb348	; symbol stub for: __ZdlPv
000046cc	e59d2094	ldr	r2, [sp, #148]
000046d0	e58d2018	str	r2, [sp, #24]
000046d4	e3e03000	mvn	r3, #0	; 0x0
000046d8	e58d30ac	str	r3, [sp, #172]
000046dc	e59d0018	ldr	r0, [sp, #24]
000046e0	eb001ae5	bl	0xb27c	; symbol stub for: __Unwind_SjLj_Resume
000046e4	e59f3500	ldr	r3, [pc, #1280]	; 0x4bec
000046e8	e08f3003	add	r3, pc, r3
000046ec	e59d2058	ldr	r2, [sp, #88]
000046f0	e5832000	str	r2, [r3]
000046f4	e3e03000	mvn	r3, #0	; 0x0
000046f8	e58d30ac	str	r3, [sp, #172]
000046fc	eb001ae4	bl	0xb294	; symbol stub for: __ZN6google8protobuf14DescriptorPool14generated_poolEv
00004700	e1a03000	mov	r3, r0
00004704	e58d305c	str	r3, [sp, #92]
00004708	e3a00024	mov	r0, #36	; 0x24
0000470c	eb001b10	bl	0xb354	; symbol stub for: __Znwm
00004710	e1a03000	mov	r3, r0
00004714	e58d3060	str	r3, [sp, #96]
00004718	e59f34d0	ldr	r3, [pc, #1232]	; 0x4bf0
0000471c	e08f3003	add	r3, pc, r3
00004720	e5931000	ldr	r1, [r3]
00004724	e59f34c8	ldr	r3, [pc, #1224]	; 0x4bf4
00004728	e08f3003	add	r3, pc, r3
0000472c	e5933000	ldr	r3, [r3]
00004730	e1a0c003	mov	ip, r3
00004734	e3a03010	mov	r3, #16	; 0x10
00004738	e2833010	add	r3, r3, #16	; 0x10
0000473c	e2432010	sub	r2, r3, #16	; 0x10
00004740	e3a03010	mov	r3, #16	; 0x10
00004744	e2833004	add	r3, r3, #4	; 0x4
00004748	e2433010	sub	r3, r3, #16	; 0x10
0000474c	e58d2000	str	r2, [sp]
00004750	e58d3004	str	r3, [sp, #4]
00004754	e3e03000	mvn	r3, #0	; 0x0
00004758	e58d3008	str	r3, [sp, #8]
0000475c	e59d305c	ldr	r3, [sp, #92]
00004760	e58d300c	str	r3, [sp, #12]
00004764	e3a03014	mov	r3, #20	; 0x14
00004768	e58d3010	str	r3, [sp, #16]
0000476c	e3a03003	mov	r3, #3	; 0x3
00004770	e58d30ac	str	r3, [sp, #172]
00004774	e59d0060	ldr	r0, [sp, #96]
00004778	e1a0200c	mov	r2, ip
0000477c	e59f3474	ldr	r3, [pc, #1140]	; 0x4bf8
00004780	e08f3003	add	r3, pc, r3
00004784	eb001add	bl	0xb300	; symbol stub for: __ZN6google8protobuf8internal26GeneratedMessageReflectionC1EPKNS0_10DescriptorEPKNS0_7MessageEPKiiiiPKNS0_14DescriptorPoolEi
00004788	ea000009	b	0x47b4
0000478c	e59d2018	ldr	r2, [sp, #24]
00004790	e58d2098	str	r2, [sp, #152]
00004794	e59d0058	ldr	r0, [sp, #88]
00004798	eb001aea	bl	0xb348	; symbol stub for: __ZdlPv
0000479c	e59d3098	ldr	r3, [sp, #152]
000047a0	e58d3018	str	r3, [sp, #24]
000047a4	e3e03000	mvn	r3, #0	; 0x0
000047a8	e58d30ac	str	r3, [sp, #172]
000047ac	e59d0018	ldr	r0, [sp, #24]
000047b0	eb001ab1	bl	0xb27c	; symbol stub for: __Unwind_SjLj_Resume
000047b4	e59f3440	ldr	r3, [pc, #1088]	; 0x4bfc
000047b8	e08f3003	add	r3, pc, r3
000047bc	e59d2060	ldr	r2, [sp, #96]
000047c0	e5832000	str	r2, [r3]
000047c4	e59f3434	ldr	r3, [pc, #1076]	; 0x4c00
000047c8	e08f3003	add	r3, pc, r3
000047cc	e5931000	ldr	r1, [r3]
000047d0	e59f342c	ldr	r3, [pc, #1068]	; 0x4c04
000047d4	e08f3003	add	r3, pc, r3
000047d8	e5933000	ldr	r3, [r3]
000047dc	e1a02003	mov	r2, r3
000047e0	e3e03000	mvn	r3, #0	; 0x0
000047e4	e58d30ac	str	r3, [sp, #172]
000047e8	e1a00001	mov	r0, r1
000047ec	e1a01002	mov	r1, r2
000047f0	eb001ab0	bl	0xb2b8	; symbol stub for: __ZN6google8protobuf14MessageFactory32InternalRegisterGeneratedMessageEPKNS0_10DescriptorEPKNS0_7MessageE
000047f4	e59d00dc	ldr	r0, [sp, #220]
000047f8	e3a01006	mov	r1, #6	; 0x6
000047fc	e59f3404	ldr	r3, [pc, #1028]	; 0x4c08
00004800	e08f3003	add	r3, pc, r3
00004804	e5933000	ldr	r3, [r3]
00004808	e12fff33	blx	r3
0000480c	e1a02000	mov	r2, r0
00004810	e59f33f4	ldr	r3, [pc, #1012]	; 0x4c0c
00004814	e08f3003	add	r3, pc, r3
00004818	e5832000	str	r2, [r3]
0000481c	e3a00040	mov	r0, #64	; 0x40
00004820	eb001acb	bl	0xb354	; symbol stub for: __Znwm
00004824	e1a03000	mov	r3, r0
00004828	e58d3064	str	r3, [sp, #100]
0000482c	e3a03002	mov	r3, #2	; 0x2
00004830	e58d30ac	str	r3, [sp, #172]
00004834	e59d0064	ldr	r0, [sp, #100]
00004838	ebfffb32	bl	__ZN3dsp15LocationMessageC1Ev
0000483c	ea000009	b	0x4868
00004840	e59d3018	ldr	r3, [sp, #24]
00004844	e58d309c	str	r3, [sp, #156]
00004848	e59d0060	ldr	r0, [sp, #96]
0000484c	eb001abd	bl	0xb348	; symbol stub for: __ZdlPv
00004850	e59d209c	ldr	r2, [sp, #156]
00004854	e58d2018	str	r2, [sp, #24]
00004858	e3e03000	mvn	r3, #0	; 0x0
0000485c	e58d30ac	str	r3, [sp, #172]
00004860	e59d0018	ldr	r0, [sp, #24]
00004864	eb001a84	bl	0xb27c	; symbol stub for: __Unwind_SjLj_Resume
00004868	e59f33a0	ldr	r3, [pc, #928]	; 0x4c10
0000486c	e08f3003	add	r3, pc, r3
00004870	e59d2064	ldr	r2, [sp, #100]
00004874	e5832000	str	r2, [r3]
00004878	e3e03000	mvn	r3, #0	; 0x0
0000487c	e58d30ac	str	r3, [sp, #172]
00004880	eb001a83	bl	0xb294	; symbol stub for: __ZN6google8protobuf14DescriptorPool14generated_poolEv
00004884	e1a03000	mov	r3, r0
00004888	e58d3068	str	r3, [sp, #104]
0000488c	e3a00024	mov	r0, #36	; 0x24
00004890	eb001aaf	bl	0xb354	; symbol stub for: __Znwm
00004894	e1a03000	mov	r3, r0
00004898	e58d306c	str	r3, [sp, #108]
0000489c	e59f3370	ldr	r3, [pc, #880]	; 0x4c14
000048a0	e08f3003	add	r3, pc, r3
000048a4	e5931000	ldr	r1, [r3]
000048a8	e59f3368	ldr	r3, [pc, #872]	; 0x4c18
000048ac	e08f3003	add	r3, pc, r3
000048b0	e5933000	ldr	r3, [r3]
000048b4	e1a0c003	mov	ip, r3
000048b8	e3a03010	mov	r3, #16	; 0x10
000048bc	e283303c	add	r3, r3, #60	; 0x3c
000048c0	e2432010	sub	r2, r3, #16	; 0x10
000048c4	e3a03010	mov	r3, #16	; 0x10
000048c8	e2833004	add	r3, r3, #4	; 0x4
000048cc	e2433010	sub	r3, r3, #16	; 0x10
000048d0	e58d2000	str	r2, [sp]
000048d4	e58d3004	str	r3, [sp, #4]
000048d8	e3e03000	mvn	r3, #0	; 0x0
000048dc	e58d3008	str	r3, [sp, #8]
000048e0	e59d3068	ldr	r3, [sp, #104]
000048e4	e58d300c	str	r3, [sp, #12]
000048e8	e3a03040	mov	r3, #64	; 0x40
000048ec	e58d3010	str	r3, [sp, #16]
000048f0	e3a03001	mov	r3, #1	; 0x1
000048f4	e58d30ac	str	r3, [sp, #172]
000048f8	e59d006c	ldr	r0, [sp, #108]
000048fc	e1a0200c	mov	r2, ip
00004900	e59f3314	ldr	r3, [pc, #788]	; 0x4c1c
00004904	e08f3003	add	r3, pc, r3
00004908	eb001a7c	bl	0xb300	; symbol stub for: __ZN6google8protobuf8internal26GeneratedMessageReflectionC1EPKNS0_10DescriptorEPKNS0_7MessageEPKiiiiPKNS0_14DescriptorPoolEi
0000490c	ea000009	b	0x4938
00004910	e59d2018	ldr	r2, [sp, #24]
00004914	e58d20a0	str	r2, [sp, #160]
00004918	e59d0064	ldr	r0, [sp, #100]
0000491c	eb001a89	bl	0xb348	; symbol stub for: __ZdlPv
00004920	e59d30a0	ldr	r3, [sp, #160]
00004924	e58d3018	str	r3, [sp, #24]
00004928	e3e03000	mvn	r3, #0	; 0x0
0000492c	e58d30ac	str	r3, [sp, #172]
00004930	e59d0018	ldr	r0, [sp, #24]
00004934	eb001a50	bl	0xb27c	; symbol stub for: __Unwind_SjLj_Resume
00004938	ea000034	b	0x4a10
0000493c	e59d20ac	ldr	r2, [sp, #172]
00004940	e58d2014	str	r2, [sp, #20]
00004944	e59d30b0	ldr	r3, [sp, #176]
00004948	e58d3018	str	r3, [sp, #24]
0000494c	e59d2014	ldr	r2, [sp, #20]
00004950	e3520001	cmp	r2, #1	; 0x1
00004954	0affffed	beq	0x4910
00004958	e59d3014	ldr	r3, [sp, #20]
0000495c	e3530002	cmp	r3, #2	; 0x2
00004960	0affffb6	beq	0x4840
00004964	e59d2014	ldr	r2, [sp, #20]
00004968	e3520003	cmp	r2, #3	; 0x3
0000496c	0affff86	beq	0x478c
00004970	e59d3014	ldr	r3, [sp, #20]
00004974	e3530004	cmp	r3, #4	; 0x4
00004978	0affff4f	beq	0x46bc
0000497c	e59d2014	ldr	r2, [sp, #20]
00004980	e3520005	cmp	r2, #5	; 0x5
00004984	0affff1f	beq	0x4608
00004988	e59d3014	ldr	r3, [sp, #20]
0000498c	e3530006	cmp	r3, #6	; 0x6
00004990	0afffee8	beq	0x4538
00004994	e59d2014	ldr	r2, [sp, #20]
00004998	e3520007	cmp	r2, #7	; 0x7
0000499c	0afffeb8	beq	0x4484
000049a0	e59d3014	ldr	r3, [sp, #20]
000049a4	e3530008	cmp	r3, #8	; 0x8
000049a8	0afffe81	beq	0x43b4
000049ac	e59d2014	ldr	r2, [sp, #20]
000049b0	e3520009	cmp	r2, #9	; 0x9
000049b4	0afffe51	beq	0x4300
000049b8	e59d3014	ldr	r3, [sp, #20]
000049bc	e353000a	cmp	r3, #10	; 0xa
000049c0	0afffe1a	beq	0x4230
000049c4	e59d2014	ldr	r2, [sp, #20]
000049c8	e352000b	cmp	r2, #11	; 0xb
000049cc	0afffdea	beq	0x417c
000049d0	e59d3014	ldr	r3, [sp, #20]
000049d4	e353000c	cmp	r3, #12	; 0xc
000049d8	0afffdb3	beq	0x40ac
000049dc	e59d2014	ldr	r2, [sp, #20]
000049e0	e352000d	cmp	r2, #13	; 0xd
000049e4	0afffd83	beq	0x3ff8
000049e8	e59d3018	ldr	r3, [sp, #24]
000049ec	e58d30a4	str	r3, [sp, #164]
000049f0	e59d006c	ldr	r0, [sp, #108]
000049f4	eb001a53	bl	0xb348	; symbol stub for: __ZdlPv
000049f8	e59d20a4	ldr	r2, [sp, #164]
000049fc	e58d2018	str	r2, [sp, #24]
00004a00	e3e03000	mvn	r3, #0	; 0x0
00004a04	e58d30ac	str	r3, [sp, #172]
00004a08	e59d0018	ldr	r0, [sp, #24]
00004a0c	eb001a1a	bl	0xb27c	; symbol stub for: __Unwind_SjLj_Resume
00004a10	e59f3208	ldr	r3, [pc, #520]	; 0x4c20
00004a14	e08f3003	add	r3, pc, r3
00004a18	e59d206c	ldr	r2, [sp, #108]
00004a1c	e5832000	str	r2, [r3]
00004a20	e59f31fc	ldr	r3, [pc, #508]	; 0x4c24
00004a24	e08f3003	add	r3, pc, r3
00004a28	e5931000	ldr	r1, [r3]
00004a2c	e59f31f4	ldr	r3, [pc, #500]	; 0x4c28
00004a30	e08f3003	add	r3, pc, r3
00004a34	e5933000	ldr	r3, [r3]
00004a38	e1a02003	mov	r2, r3
00004a3c	e3e03000	mvn	r3, #0	; 0x0
00004a40	e58d30ac	str	r3, [sp, #172]
00004a44	e1a00001	mov	r0, r1
00004a48	e1a01002	mov	r1, r2
00004a4c	eb001a19	bl	0xb2b8	; symbol stub for: __ZN6google8protobuf14MessageFactory32InternalRegisterGeneratedMessageEPKNS0_10DescriptorEPKNS0_7MessageE
00004a50	e59d00dc	ldr	r0, [sp, #220]
00004a54	e3a01000	mov	r1, #0	; 0x0
00004a58	e59f31cc	ldr	r3, [pc, #460]	; 0x4c2c
00004a5c	e08f3003	add	r3, pc, r3
00004a60	e5933000	ldr	r3, [r3]
00004a64	e12fff33	blx	r3
00004a68	e1a02000	mov	r2, r0
00004a6c	e59f31bc	ldr	r3, [pc, #444]	; 0x4c30
00004a70	e08f3003	add	r3, pc, r3
00004a74	e5832000	str	r2, [r3]
00004a78	e59f31b4	ldr	r3, [pc, #436]	; 0x4c34
00004a7c	e08f3003	add	r3, pc, r3
00004a80	e5933000	ldr	r3, [r3]
00004a84	e1a00003	mov	r0, r3
00004a88	ebfffa2e	bl	__ZN3dsp6Header21InitAsDefaultInstanceEv
00004a8c	e59f31a4	ldr	r3, [pc, #420]	; 0x4c38
00004a90	e08f3003	add	r3, pc, r3
00004a94	e5933000	ldr	r3, [r3]
00004a98	e1a00003	mov	r0, r3
00004a9c	ebfffa2f	bl	__ZN3dsp24AccelerometerDescription21InitAsDefaultInstanceEv
00004aa0	e59f3194	ldr	r3, [pc, #404]	; 0x4c3c
00004aa4	e08f3003	add	r3, pc, r3
00004aa8	e5933000	ldr	r3, [r3]
00004aac	e1a00003	mov	r0, r3
00004ab0	ebfffa30	bl	__ZN3dsp20AccelerometerMessage21InitAsDefaultInstanceEv
00004ab4	e59f3184	ldr	r3, [pc, #388]	; 0x4c40
00004ab8	e08f3003	add	r3, pc, r3
00004abc	e5933000	ldr	r3, [r3]
00004ac0	e1a00003	mov	r0, r3
00004ac4	ebfffa31	bl	__ZN3dsp21MicrophoneDescription21InitAsDefaultInstanceEv
00004ac8	e59f3174	ldr	r3, [pc, #372]	; 0x4c44
00004acc	e08f3003	add	r3, pc, r3
00004ad0	e5933000	ldr	r3, [r3]
00004ad4	e1a00003	mov	r0, r3
00004ad8	ebfffa32	bl	__ZN3dsp17MicrophoneMessage21InitAsDefaultInstanceEv
00004adc	e59f3164	ldr	r3, [pc, #356]	; 0x4c48
00004ae0	e08f3003	add	r3, pc, r3
00004ae4	e5933000	ldr	r3, [r3]
00004ae8	e1a00003	mov	r0, r3
00004aec	ebfffa33	bl	__ZN3dsp19LocationDescription21InitAsDefaultInstanceEv
00004af0	e59f3154	ldr	r3, [pc, #340]	; 0x4c4c
00004af4	e08f3003	add	r3, pc, r3
00004af8	e5933000	ldr	r3, [r3]
00004afc	e1a00003	mov	r0, r3
00004b00	ebfffa34	bl	__ZN3dsp15LocationMessage21InitAsDefaultInstanceEv
00004b04	e28d30a8	add	r3, sp, #168	; 0xa8
00004b08	e1a00003	mov	r0, r3
00004b0c	eb0019dd	bl	0xb288	; symbol stub for: __Unwind_SjLj_Unregister
00004b10	e247d05c	sub	sp, r7, #92	; 0x5c
00004b14	ecbd8b11	fldmiax	sp!, {d8-d15}
00004b18	e247d018	sub	sp, r7, #24	; 0x18
00004b1c	e8bd0d00	ldmia	sp!, {r8, r10, r11}
00004b20	e8bd80f0	ldmia	sp!, {r4, r5, r6, r7, pc}
00004b24	00008154	andeq	r8, r0, r4, asr r1
00004b28	0000865e	andeq	r8, r0, lr, asr r6
00004b2c	00000a4c	andeq	r0, r0, ip, asr #20
00004b30	00008118	andeq	r8, r0, r8, lsl r1
00004b34	000081f8	streqd	r8, [r0], -r8
00004b38	00008188	andeq	r8, r0, r8, lsl #3
00004b3c	00008190	muleq	r0, r0, r1
00004b40	00008148	andeq	r8, r0, r8, asr #2
00004b44	00008508	andeq	r8, r0, r8, lsl #10
00004b48	000080f0	streqd	r8, [r0], -r0
00004b4c	000080e4	andeq	r8, r0, r4, ror #1
00004b50	0000809c	muleq	r0, ip, r0
00004b54	00007fb8	streqh	r7, [r0], -r8
00004b58	00008090	muleq	r0, r0, r0
00004b5c	00008000	andeq	r8, r0, r0
00004b60	00008004	andeq	r8, r0, r4
00004b64	00007fc0	andeq	r7, r0, r0, asr #31
00004b68	00008380	andeq	r8, r0, r0, lsl #7
00004b6c	00007f64	andeq	r7, r0, r4, ror #30
00004b70	00007f58	andeq	r7, r0, r8, asr pc
00004b74	00007f14	andeq	r7, r0, r4, lsl pc
00004b78	00007e34	andeq	r7, r0, r4, lsr lr
00004b7c	00007f04	andeq	r7, r0, r4, lsl #30
00004b80	00007e78	andeq	r7, r0, r8, ror lr
00004b84	00007e78	andeq	r7, r0, r8, ror lr
00004b88	00007e38	andeq	r7, r0, r8, lsr lr
00004b8c	000081ec	andeq	r8, r0, ip, ror #3
00004b90	00007dd8	ldreqd	r7, [r0], -r8
00004b94	00007dcc	andeq	r7, r0, ip, asr #27
00004b98	00007d8c	andeq	r7, r0, ip, lsl #27
00004b9c	00007cb0	streqh	r7, [r0], -r0
00004ba0	00007d78	andeq	r7, r0, r8, ror sp
00004ba4	00007cf0	streqd	r7, [r0], -r0
00004ba8	00007cec	andeq	r7, r0, ip, ror #25
00004bac	00007cb0	streqh	r7, [r0], -r0
00004bb0	0000805c	andeq	r8, r0, ip, asr r0
00004bb4	00007c4c	andeq	r7, r0, ip, asr #24
00004bb8	00007c40	andeq	r7, r0, r0, asr #24
00004bbc	00007c04	andeq	r7, r0, r4, lsl #24
00004bc0	00007b2c	andeq	r7, r0, ip, lsr #22
00004bc4	00007bec	andeq	r7, r0, ip, ror #23
00004bc8	00007b68	andeq	r7, r0, r8, ror #22
00004bcc	00007b60	andeq	r7, r0, r0, ror #22
00004bd0	00007b28	andeq	r7, r0, r8, lsr #22
00004bd4	00007ed0	ldreqd	r7, [r0], -r0
00004bd8	00007ac0	andeq	r7, r0, r0, asr #21
00004bdc	00007ab4	streqh	r7, [r0], -r4
00004be0	00007a7c	andeq	r7, r0, ip, ror r10
00004be4	000079a8	andeq	r7, r0, r8, lsr #19
00004be8	00007a60	andeq	r7, r0, r0, ror #20
00004bec	000079e0	andeq	r7, r0, r0, ror #19
00004bf0	000079d4	ldreqd	r7, [r0], -r4
00004bf4	000079a0	andeq	r7, r0, r0, lsr #19
00004bf8	00007d48	andeq	r7, r0, r8, asr #26
00004bfc	00007934	andeq	r7, r0, r4, lsr r9
00004c00	00007928	andeq	r7, r0, r8, lsr #18
00004c04	000078f4	streqd	r7, [r0], -r4
00004c08	00007824	andeq	r7, r0, r4, lsr #16
00004c0c	000078d4	ldreqd	r7, [r0], -r4
00004c10	00007858	andeq	r7, r0, r8, asr r8
00004c14	00007848	andeq	r7, r0, r8, asr #16
00004c18	00007818	andeq	r7, r0, r8, lsl r8
00004c1c	00007bac	andeq	r7, r0, ip, lsr #23
00004c20	000076d0	ldreqd	r7, [r0], -r0
00004c24	000076c4	andeq	r7, r0, r4, asr #13
00004c28	00007694	muleq	r0, r4, r6
00004c2c	000075cc	andeq	r7, r0, ip, asr #11
00004c30	00007670	andeq	r7, r0, r0, ror r6
00004c34	00007660	andeq	r7, r0, r0, ror #12
00004c38	00007648	andeq	r7, r0, r8, asr #12
00004c3c	00007630	andeq	r7, r0, r0, lsr r6
00004c40	00007618	andeq	r7, r0, r8, lsl r6
00004c44	00007600	andeq	r7, r0, r0, lsl #12
00004c48	000075e8	andeq	r7, r0, r8, ror #11
00004c4c	000075d0	ldreqd	r7, [r0], -r0
00004c50	e1a00000	nop			(mov r0,r0)
__ZN3dsp6HeaderC2Ev:
00004c54	e92d40f0	stmdb	sp!, {r4, r5, r6, r7, lr}
00004c58	e28d700c	add	r7, sp, #12	; 0xc
00004c5c	e92d0d00	stmdb	sp!, {r8, r10, r11}
00004c60	ed2d8b11	fstmdbx	sp!, {d8-d15}
00004c64	e24dd040	sub	sp, sp, #64	; 0x40
00004c68	e58d003c	str	r0, [sp, #60]
00004c6c	e59f3134	ldr	r3, [pc, #308]	; 0x4da8
00004c70	e08f3003	add	r3, pc, r3
00004c74	e5933000	ldr	r3, [r3]
00004c78	e58d3020	str	r3, [sp, #32]
00004c7c	e59f3128	ldr	r3, [pc, #296]	; 0x4dac
00004c80	e08f3003	add	r3, pc, r3
00004c84	e58d3024	str	r3, [sp, #36]
00004c88	e28d2028	add	r2, sp, #40	; 0x28
00004c8c	e5827000	str	r7, [r2]
00004c90	e59f3118	ldr	r3, [pc, #280]	; 0x4db0
00004c94	e08f3003	add	r3, pc, r3
00004c98	e5823004	str	r3, [r2, #4]
00004c9c	e582d008	str	sp, [r2, #8]
00004ca0	e28d3008	add	r3, sp, #8	; 0x8
00004ca4	e1a00003	mov	r0, r3
00004ca8	eb001970	bl	0xb270	; symbol stub for: __Unwind_SjLj_Register
00004cac	e59d303c	ldr	r3, [sp, #60]
00004cb0	e1a00003	mov	r0, r3
00004cb4	e59f30f8	ldr	r3, [pc, #248]	; 0x4db4
00004cb8	e08f3003	add	r3, pc, r3
00004cbc	e5933000	ldr	r3, [r3]
00004cc0	e12fff33	blx	r3
00004cc4	e59f30ec	ldr	r3, [pc, #236]	; 0x4db8
00004cc8	e08f3003	add	r3, pc, r3
00004ccc	e2832008	add	r2, r3, #8	; 0x8
00004cd0	e59d303c	ldr	r3, [sp, #60]
00004cd4	e5832000	str	r2, [r3]
00004cd8	e59d303c	ldr	r3, [sp, #60]
00004cdc	e2832004	add	r2, r3, #4	; 0x4
00004ce0	e3a03001	mov	r3, #1	; 0x1
00004ce4	e58d300c	str	r3, [sp, #12]
00004ce8	e1a00002	mov	r0, r2
00004cec	eb001974	bl	0xb2c4	; symbol stub for: __ZN6google8protobuf15UnknownFieldSetC1Ev
00004cf0	e59d203c	ldr	r2, [sp, #60]
00004cf4	e3a03000	mov	r3, #0	; 0x0
00004cf8	e5823008	str	r3, [r2, #8]
00004cfc	e59d203c	ldr	r2, [sp, #60]
00004d00	e59f30b4	ldr	r3, [pc, #180]	; 0x4dbc
00004d04	e08f3003	add	r3, pc, r3
00004d08	e582300c	str	r3, [r2, #12]
00004d0c	e59d203c	ldr	r2, [sp, #60]
00004d10	e3a03000	mov	r3, #0	; 0x0
00004d14	e5823010	str	r3, [r2, #16]
00004d18	e59d203c	ldr	r2, [sp, #60]
00004d1c	e3a03000	mov	r3, #0	; 0x0
00004d20	e5823014	str	r3, [r2, #20]
00004d24	e59d103c	ldr	r1, [sp, #60]
00004d28	e3a02000	mov	r2, #0	; 0x0
00004d2c	e3a03000	mov	r3, #0	; 0x0
00004d30	e5812018	str	r2, [r1, #24]
00004d34	e581301c	str	r3, [r1, #28]
00004d38	e59d303c	ldr	r3, [sp, #60]
00004d3c	e2832020	add	r2, r3, #32	; 0x20
00004d40	e3a03000	mov	r3, #0	; 0x0
00004d44	e5823000	str	r3, [r2]
00004d48	ea00000e	b	0x4d88
00004d4c	e59d3010	ldr	r3, [sp, #16]
00004d50	e58d3000	str	r3, [sp]
00004d54	e59d3000	ldr	r3, [sp]
00004d58	e58d3004	str	r3, [sp, #4]
00004d5c	e59d203c	ldr	r2, [sp, #60]
00004d60	e3a03000	mov	r3, #0	; 0x0
00004d64	e58d300c	str	r3, [sp, #12]
00004d68	e1a00002	mov	r0, r2
00004d6c	eb00195d	bl	0xb2e8	; symbol stub for: __ZN6google8protobuf7MessageD2Ev
00004d70	e59d3004	ldr	r3, [sp, #4]
00004d74	e58d3000	str	r3, [sp]
00004d78	e3e03000	mvn	r3, #0	; 0x0
00004d7c	e58d300c	str	r3, [sp, #12]
00004d80	e59d0000	ldr	r0, [sp]
00004d84	eb00193c	bl	0xb27c	; symbol stub for: __Unwind_SjLj_Resume
00004d88	e28d3008	add	r3, sp, #8	; 0x8
00004d8c	e1a00003	mov	r0, r3
00004d90	eb00193c	bl	0xb288	; symbol stub for: __Unwind_SjLj_Unregister
00004d94	e247d05c	sub	sp, r7, #92	; 0x5c
00004d98	ecbd8b11	fldmiax	sp!, {d8-d15}
00004d9c	e247d018	sub	sp, r7, #24	; 0x18
00004da0	e8bd0d00	ldmia	sp!, {r8, r10, r11}
00004da4	e8bd80f0	ldmia	sp!, {r4, r5, r6, r7, pc}
00004da8	000073a8	andeq	r7, r0, r8, lsr #7
00004dac	000078d2	ldreqd	r7, [r0], -r2
00004db0	000000b0	streqh	r0, [r0], -r0
00004db4	00007368	andeq	r7, r0, r8, ror #6
00004db8	000075f0	streqd	r7, [r0], -r0
00004dbc	000079d8	ldreqd	r7, [r0], -r8
__ZN3dsp6HeaderC2ERKS0_:
00004dc0	e92d40f0	stmdb	sp!, {r4, r5, r6, r7, lr}
00004dc4	e28d700c	add	r7, sp, #12	; 0xc
00004dc8	e92d0d00	stmdb	sp!, {r8, r10, r11}
00004dcc	ed2d8b11	fstmdbx	sp!, {d8-d15}
00004dd0	e24dd048	sub	sp, sp, #72	; 0x48
00004dd4	e58d0044	str	r0, [sp, #68]
00004dd8	e58d1040	str	r1, [sp, #64]
00004ddc	e59f3180	ldr	r3, [pc, #384]	; 0x4f64
00004de0	e08f3003	add	r3, pc, r3
00004de4	e5933000	ldr	r3, [r3]
00004de8	e58d3024	str	r3, [sp, #36]
00004dec	e59f3174	ldr	r3, [pc, #372]	; 0x4f68
00004df0	e08f3003	add	r3, pc, r3
00004df4	e58d3028	str	r3, [sp, #40]
00004df8	e28d202c	add	r2, sp, #44	; 0x2c
00004dfc	e5827000	str	r7, [r2]
00004e00	e59f3164	ldr	r3, [pc, #356]	; 0x4f6c
00004e04	e08f3003	add	r3, pc, r3
00004e08	e5823004	str	r3, [r2, #4]
00004e0c	e582d008	str	sp, [r2, #8]
00004e10	e28d300c	add	r3, sp, #12	; 0xc
00004e14	e1a00003	mov	r0, r3
00004e18	eb001914	bl	0xb270	; symbol stub for: __Unwind_SjLj_Register
00004e1c	e59d3044	ldr	r3, [sp, #68]
00004e20	e1a00003	mov	r0, r3
00004e24	e59f3144	ldr	r3, [pc, #324]	; 0x4f70
00004e28	e08f3003	add	r3, pc, r3
00004e2c	e5933000	ldr	r3, [r3]
00004e30	e12fff33	blx	r3
00004e34	e59f3138	ldr	r3, [pc, #312]	; 0x4f74
00004e38	e08f3003	add	r3, pc, r3
00004e3c	e2832008	add	r2, r3, #8	; 0x8
00004e40	e59d3044	ldr	r3, [sp, #68]
00004e44	e5832000	str	r2, [r3]
00004e48	e59d3044	ldr	r3, [sp, #68]
00004e4c	e2832004	add	r2, r3, #4	; 0x4
00004e50	e3a03002	mov	r3, #2	; 0x2
00004e54	e58d3010	str	r3, [sp, #16]
00004e58	e1a00002	mov	r0, r2
00004e5c	eb001918	bl	0xb2c4	; symbol stub for: __ZN6google8protobuf15UnknownFieldSetC1Ev
00004e60	e59d2044	ldr	r2, [sp, #68]
00004e64	e3a03000	mov	r3, #0	; 0x0
00004e68	e5823008	str	r3, [r2, #8]
00004e6c	e59d2044	ldr	r2, [sp, #68]
00004e70	e59f3100	ldr	r3, [pc, #256]	; 0x4f78
00004e74	e08f3003	add	r3, pc, r3
00004e78	e582300c	str	r3, [r2, #12]
00004e7c	e59d2044	ldr	r2, [sp, #68]
00004e80	e3a03000	mov	r3, #0	; 0x0
00004e84	e5823010	str	r3, [r2, #16]
00004e88	e59d2044	ldr	r2, [sp, #68]
00004e8c	e3a03000	mov	r3, #0	; 0x0
00004e90	e5823014	str	r3, [r2, #20]
00004e94	e59d1044	ldr	r1, [sp, #68]
00004e98	e3a02000	mov	r2, #0	; 0x0
00004e9c	e3a03000	mov	r3, #0	; 0x0
00004ea0	e5812018	str	r2, [r1, #24]
00004ea4	e581301c	str	r3, [r1, #28]
00004ea8	e59d3044	ldr	r3, [sp, #68]
00004eac	e2832020	add	r2, r3, #32	; 0x20
00004eb0	e3a03000	mov	r3, #0	; 0x0
00004eb4	e5823000	str	r3, [r2]
00004eb8	e59d2044	ldr	r2, [sp, #68]
00004ebc	e59d1040	ldr	r1, [sp, #64]
00004ec0	e3a03001	mov	r3, #1	; 0x1
00004ec4	e58d3010	str	r3, [sp, #16]
00004ec8	e1a00002	mov	r0, r2
00004ecc	eb001902	bl	0xb2dc	; symbol stub for: __ZN6google8protobuf7Message9MergeFromERKS1_
00004ed0	ea00001b	b	0x4f44
00004ed4	e59d3010	ldr	r3, [sp, #16]
00004ed8	e59d2014	ldr	r2, [sp, #20]
00004edc	e58d2000	str	r2, [sp]
00004ee0	e3530001	cmp	r3, #1	; 0x1
00004ee4	0a000009	beq	0x4f10
00004ee8	e59d3000	ldr	r3, [sp]
00004eec	e58d3004	str	r3, [sp, #4]
00004ef0	e59d3044	ldr	r3, [sp, #68]
00004ef4	e2832004	add	r2, r3, #4	; 0x4
00004ef8	e3a03000	mov	r3, #0	; 0x0
00004efc	e58d3010	str	r3, [sp, #16]
00004f00	e1a00002	mov	r0, r2
00004f04	eb0018f1	bl	0xb2d0	; symbol stub for: __ZN6google8protobuf15UnknownFieldSetD1Ev
00004f08	e59d2004	ldr	r2, [sp, #4]
00004f0c	e58d2000	str	r2, [sp]
00004f10	e59d3000	ldr	r3, [sp]
00004f14	e58d3008	str	r3, [sp, #8]
00004f18	e59d2044	ldr	r2, [sp, #68]
00004f1c	e3a03000	mov	r3, #0	; 0x0
00004f20	e58d3010	str	r3, [sp, #16]
00004f24	e1a00002	mov	r0, r2
00004f28	eb0018ee	bl	0xb2e8	; symbol stub for: __ZN6google8protobuf7MessageD2Ev
00004f2c	e59d2008	ldr	r2, [sp, #8]
00004f30	e58d2000	str	r2, [sp]
00004f34	e3e03000	mvn	r3, #0	; 0x0
00004f38	e58d3010	str	r3, [sp, #16]
00004f3c	e59d0000	ldr	r0, [sp]
00004f40	eb0018cd	bl	0xb27c	; symbol stub for: __Unwind_SjLj_Resume
00004f44	e28d300c	add	r3, sp, #12	; 0xc
00004f48	e1a00003	mov	r0, r3
00004f4c	eb0018cd	bl	0xb288	; symbol stub for: __Unwind_SjLj_Unregister
00004f50	e247d05c	sub	sp, r7, #92	; 0x5c
00004f54	ecbd8b11	fldmiax	sp!, {d8-d15}
00004f58	e247d018	sub	sp, r7, #24	; 0x18
00004f5c	e8bd0d00	ldmia	sp!, {r8, r10, r11}
00004f60	e8bd80f0	ldmia	sp!, {r4, r5, r6, r7, pc}
00004f64	00007238	andeq	r7, r0, r8, lsr r2
00004f68	00007768	andeq	r7, r0, r8, ror #14
00004f6c	000000c8	andeq	r0, r0, r8, asr #1
00004f70	000071f8	streqd	r7, [r0], -r8
00004f74	00007480	andeq	r7, r0, r0, lsl #9
00004f78	00007868	andeq	r7, r0, r8, ror #16
__ZN3dsp6HeaderC1ERKS0_:
00004f7c	e92d40f0	stmdb	sp!, {r4, r5, r6, r7, lr}
00004f80	e28d700c	add	r7, sp, #12	; 0xc
00004f84	e92d0d00	stmdb	sp!, {r8, r10, r11}
00004f88	ed2d8b11	fstmdbx	sp!, {d8-d15}
00004f8c	e24dd048	sub	sp, sp, #72	; 0x48
00004f90	e58d0044	str	r0, [sp, #68]
00004f94	e58d1040	str	r1, [sp, #64]
00004f98	e59f3180	ldr	r3, [pc, #384]	; 0x5120
00004f9c	e08f3003	add	r3, pc, r3
00004fa0	e5933000	ldr	r3, [r3]
00004fa4	e58d3024	str	r3, [sp, #36]
00004fa8	e59f3174	ldr	r3, [pc, #372]	; 0x5124
00004fac	e08f3003	add	r3, pc, r3
00004fb0	e58d3028	str	r3, [sp, #40]
00004fb4	e28d202c	add	r2, sp, #44	; 0x2c
00004fb8	e5827000	str	r7, [r2]
00004fbc	e59f3164	ldr	r3, [pc, #356]	; 0x5128
00004fc0	e08f3003	add	r3, pc, r3
00004fc4	e5823004	str	r3, [r2, #4]
00004fc8	e582d008	str	sp, [r2, #8]
00004fcc	e28d300c	add	r3, sp, #12	; 0xc
00004fd0	e1a00003	mov	r0, r3
00004fd4	eb0018a5	bl	0xb270	; symbol stub for: __Unwind_SjLj_Register
00004fd8	e59d3044	ldr	r3, [sp, #68]
00004fdc	e1a00003	mov	r0, r3
00004fe0	e59f3144	ldr	r3, [pc, #324]	; 0x512c
00004fe4	e08f3003	add	r3, pc, r3
00004fe8	e5933000	ldr	r3, [r3]
00004fec	e12fff33	blx	r3
00004ff0	e59f3138	ldr	r3, [pc, #312]	; 0x5130
00004ff4	e08f3003	add	r3, pc, r3
00004ff8	e2832008	add	r2, r3, #8	; 0x8
00004ffc	e59d3044	ldr	r3, [sp, #68]
00005000	e5832000	str	r2, [r3]
00005004	e59d3044	ldr	r3, [sp, #68]
00005008	e2832004	add	r2, r3, #4	; 0x4
0000500c	e3a03002	mov	r3, #2	; 0x2
00005010	e58d3010	str	r3, [sp, #16]
00005014	e1a00002	mov	r0, r2
00005018	eb0018a9	bl	0xb2c4	; symbol stub for: __ZN6google8protobuf15UnknownFieldSetC1Ev
0000501c	e59d2044	ldr	r2, [sp, #68]
00005020	e3a03000	mov	r3, #0	; 0x0
00005024	e5823008	str	r3, [r2, #8]
00005028	e59d2044	ldr	r2, [sp, #68]
0000502c	e59f3100	ldr	r3, [pc, #256]	; 0x5134
00005030	e08f3003	add	r3, pc, r3
00005034	e582300c	str	r3, [r2, #12]
00005038	e59d2044	ldr	r2, [sp, #68]
0000503c	e3a03000	mov	r3, #0	; 0x0
00005040	e5823010	str	r3, [r2, #16]
00005044	e59d2044	ldr	r2, [sp, #68]
00005048	e3a03000	mov	r3, #0	; 0x0
0000504c	e5823014	str	r3, [r2, #20]
00005050	e59d1044	ldr	r1, [sp, #68]
00005054	e3a02000	mov	r2, #0	; 0x0
00005058	e3a03000	mov	r3, #0	; 0x0
0000505c	e5812018	str	r2, [r1, #24]
00005060	e581301c	str	r3, [r1, #28]
00005064	e59d3044	ldr	r3, [sp, #68]
00005068	e2832020	add	r2, r3, #32	; 0x20
0000506c	e3a03000	mov	r3, #0	; 0x0
00005070	e5823000	str	r3, [r2]
00005074	e59d2044	ldr	r2, [sp, #68]
00005078	e59d1040	ldr	r1, [sp, #64]
0000507c	e3a03001	mov	r3, #1	; 0x1
00005080	e58d3010	str	r3, [sp, #16]
00005084	e1a00002	mov	r0, r2
00005088	eb001893	bl	0xb2dc	; symbol stub for: __ZN6google8protobuf7Message9MergeFromERKS1_
0000508c	ea00001b	b	0x5100
00005090	e59d3010	ldr	r3, [sp, #16]
00005094	e59d2014	ldr	r2, [sp, #20]
00005098	e58d2000	str	r2, [sp]
0000509c	e3530001	cmp	r3, #1	; 0x1
000050a0	0a000009	beq	0x50cc
000050a4	e59d3000	ldr	r3, [sp]
000050a8	e58d3004	str	r3, [sp, #4]
000050ac	e59d3044	ldr	r3, [sp, #68]
000050b0	e2832004	add	r2, r3, #4	; 0x4
000050b4	e3a03000	mov	r3, #0	; 0x0
000050b8	e58d3010	str	r3, [sp, #16]
000050bc	e1a00002	mov	r0, r2
000050c0	eb001882	bl	0xb2d0	; symbol stub for: __ZN6google8protobuf15UnknownFieldSetD1Ev
000050c4	e59d2004	ldr	r2, [sp, #4]
000050c8	e58d2000	str	r2, [sp]
000050cc	e59d3000	ldr	r3, [sp]
000050d0	e58d3008	str	r3, [sp, #8]
000050d4	e59d2044	ldr	r2, [sp, #68]
000050d8	e3a03000	mov	r3, #0	; 0x0
000050dc	e58d3010	str	r3, [sp, #16]
000050e0	e1a00002	mov	r0, r2
000050e4	eb00187f	bl	0xb2e8	; symbol stub for: __ZN6google8protobuf7MessageD2Ev
000050e8	e59d2008	ldr	r2, [sp, #8]
000050ec	e58d2000	str	r2, [sp]
000050f0	e3e03000	mvn	r3, #0	; 0x0
000050f4	e58d3010	str	r3, [sp, #16]
000050f8	e59d0000	ldr	r0, [sp]
000050fc	eb00185e	bl	0xb27c	; symbol stub for: __Unwind_SjLj_Resume
00005100	e28d300c	add	r3, sp, #12	; 0xc
00005104	e1a00003	mov	r0, r3
00005108	eb00185e	bl	0xb288	; symbol stub for: __Unwind_SjLj_Unregister
0000510c	e247d05c	sub	sp, r7, #92	; 0x5c
00005110	ecbd8b11	fldmiax	sp!, {d8-d15}
00005114	e247d018	sub	sp, r7, #24	; 0x18
00005118	e8bd0d00	ldmia	sp!, {r8, r10, r11}
0000511c	e8bd80f0	ldmia	sp!, {r4, r5, r6, r7, pc}
00005120	0000707c	andeq	r7, r0, ip, ror r0
00005124	000075b4	streqh	r7, [r0], -r4
00005128	000000c8	andeq	r0, r0, r8, asr #1
0000512c	0000703c	andeq	r7, r0, ip, lsr r0
00005130	000072c4	andeq	r7, r0, r4, asr #5
00005134	000076ac	andeq	r7, r0, ip, lsr #13
__ZN3dsp6HeaderD2Ev:
00005138	e92d40f0	stmdb	sp!, {r4, r5, r6, r7, lr}
0000513c	e28d700c	add	r7, sp, #12	; 0xc
00005140	e92d0d00	stmdb	sp!, {r8, r10, r11}
00005144	ed2d8b11	fstmdbx	sp!, {d8-d15}
00005148	e24dd050	sub	sp, sp, #80	; 0x50
0000514c	e58d004c	str	r0, [sp, #76]
00005150	e59f317c	ldr	r3, [pc, #380]	; 0x52d4
00005154	e08f3003	add	r3, pc, r3
00005158	e5933000	ldr	r3, [r3]
0000515c	e58d3030	str	r3, [sp, #48]
00005160	e59f3170	ldr	r3, [pc, #368]	; 0x52d8
00005164	e08f3003	add	r3, pc, r3
00005168	e58d3034	str	r3, [sp, #52]
0000516c	e28d2038	add	r2, sp, #56	; 0x38
00005170	e5827000	str	r7, [r2]
00005174	e59f3160	ldr	r3, [pc, #352]	; 0x52dc
00005178	e08f3003	add	r3, pc, r3
0000517c	e5823004	str	r3, [r2, #4]
00005180	e582d008	str	sp, [r2, #8]
00005184	e28d3018	add	r3, sp, #24	; 0x18
00005188	e1a00003	mov	r0, r3
0000518c	eb001837	bl	0xb270	; symbol stub for: __Unwind_SjLj_Register
00005190	e59f3148	ldr	r3, [pc, #328]	; 0x52e0
00005194	e08f3003	add	r3, pc, r3
00005198	e2832008	add	r2, r3, #8	; 0x8
0000519c	e59d304c	ldr	r3, [sp, #76]
000051a0	e5832000	str	r2, [r3]
000051a4	e59d304c	ldr	r3, [sp, #76]
000051a8	e593200c	ldr	r2, [r3, #12]
000051ac	e59f3130	ldr	r3, [pc, #304]	; 0x52e4
000051b0	e08f3003	add	r3, pc, r3
000051b4	e1520003	cmp	r2, r3
000051b8	0a00000b	beq	0x51ec
000051bc	e59d304c	ldr	r3, [sp, #76]
000051c0	e593300c	ldr	r3, [r3, #12]
000051c4	e58d3004	str	r3, [sp, #4]
000051c8	e59d2004	ldr	r2, [sp, #4]
000051cc	e3520000	cmp	r2, #0	; 0x0
000051d0	0a000005	beq	0x51ec
000051d4	e3a03001	mov	r3, #1	; 0x1
000051d8	e58d301c	str	r3, [sp, #28]
000051dc	e59d0004	ldr	r0, [sp, #4]
000051e0	eb001855	bl	0xb33c	; symbol stub for: __ZNSsD1Ev
000051e4	e59d0004	ldr	r0, [sp, #4]
000051e8	eb001856	bl	0xb348	; symbol stub for: __ZdlPv
000051ec	e59d304c	ldr	r3, [sp, #76]
000051f0	e2833004	add	r3, r3, #4	; 0x4
000051f4	e58d3008	str	r3, [sp, #8]
000051f8	e3a03002	mov	r3, #2	; 0x2
000051fc	e58d301c	str	r3, [sp, #28]
00005200	e59d0008	ldr	r0, [sp, #8]
00005204	eb001831	bl	0xb2d0	; symbol stub for: __ZN6google8protobuf15UnknownFieldSetD1Ev
00005208	e59d304c	ldr	r3, [sp, #76]
0000520c	e58d300c	str	r3, [sp, #12]
00005210	e3e03000	mvn	r3, #0	; 0x0
00005214	e58d301c	str	r3, [sp, #28]
00005218	e59d000c	ldr	r0, [sp, #12]
0000521c	eb001831	bl	0xb2e8	; symbol stub for: __ZN6google8protobuf7MessageD2Ev
00005220	e3a03000	mov	r3, #0	; 0x0
00005224	e6ef3073	uxtb r3,r3
00005228	e3530000	cmp	r3, #0	; 0x0
0000522c	0a000020	beq	0x52b4
00005230	ea00001d	b	0x52ac
00005234	e59d301c	ldr	r3, [sp, #28]
00005238	e59d2020	ldr	r2, [sp, #32]
0000523c	e58d2000	str	r2, [sp]
00005240	e3530001	cmp	r3, #1	; 0x1
00005244	0a00000a	beq	0x5274
00005248	e59d3000	ldr	r3, [sp]
0000524c	e58d3010	str	r3, [sp, #16]
00005250	e59d304c	ldr	r3, [sp, #76]
00005254	e2833004	add	r3, r3, #4	; 0x4
00005258	e58d3008	str	r3, [sp, #8]
0000525c	e3a03000	mov	r3, #0	; 0x0
00005260	e58d301c	str	r3, [sp, #28]
00005264	e59d0008	ldr	r0, [sp, #8]
00005268	eb001818	bl	0xb2d0	; symbol stub for: __ZN6google8protobuf15UnknownFieldSetD1Ev
0000526c	e59d2010	ldr	r2, [sp, #16]
00005270	e58d2000	str	r2, [sp]
00005274	e59d3000	ldr	r3, [sp]
00005278	e58d3014	str	r3, [sp, #20]
0000527c	e59d204c	ldr	r2, [sp, #76]
00005280	e58d200c	str	r2, [sp, #12]
00005284	e3a03000	mov	r3, #0	; 0x0
00005288	e58d301c	str	r3, [sp, #28]
0000528c	e59d000c	ldr	r0, [sp, #12]
00005290	eb001814	bl	0xb2e8	; symbol stub for: __ZN6google8protobuf7MessageD2Ev
00005294	e59d3014	ldr	r3, [sp, #20]
00005298	e58d3000	str	r3, [sp]
0000529c	e3e03000	mvn	r3, #0	; 0x0
000052a0	e58d301c	str	r3, [sp, #28]
000052a4	e59d0000	ldr	r0, [sp]
000052a8	eb0017f3	bl	0xb27c	; symbol stub for: __Unwind_SjLj_Resume
000052ac	e59d004c	ldr	r0, [sp, #76]
000052b0	eb001824	bl	0xb348	; symbol stub for: __ZdlPv
000052b4	e28d3018	add	r3, sp, #24	; 0x18
000052b8	e1a00003	mov	r0, r3
000052bc	eb0017f1	bl	0xb288	; symbol stub for: __Unwind_SjLj_Unregister
000052c0	e247d05c	sub	sp, r7, #92	; 0x5c
000052c4	ecbd8b11	fldmiax	sp!, {d8-d15}
000052c8	e247d018	sub	sp, r7, #24	; 0x18
000052cc	e8bd0d00	ldmia	sp!, {r8, r10, r11}
000052d0	e8bd80f0	ldmia	sp!, {r4, r5, r6, r7, pc}
000052d4	00006ec4	andeq	r6, r0, r4, asr #29
000052d8	00007404	andeq	r7, r0, r4, lsl #8
000052dc	000000b4	streqh	r0, [r0], -r4
000052e0	00007124	andeq	r7, r0, r4, lsr #2
000052e4	0000752c	andeq	r7, r0, ip, lsr #10
__ZN3dsp6HeaderD1Ev:
000052e8	e92d40f0	stmdb	sp!, {r4, r5, r6, r7, lr}
000052ec	e28d700c	add	r7, sp, #12	; 0xc
000052f0	e92d0d00	stmdb	sp!, {r8, r10, r11}
000052f4	ed2d8b11	fstmdbx	sp!, {d8-d15}
000052f8	e24dd050	sub	sp, sp, #80	; 0x50
000052fc	e58d004c	str	r0, [sp, #76]
00005300	e59f317c	ldr	r3, [pc, #380]	; 0x5484
00005304	e08f3003	add	r3, pc, r3
00005308	e5933000	ldr	r3, [r3]
0000530c	e58d3030	str	r3, [sp, #48]
00005310	e59f3170	ldr	r3, [pc, #368]	; 0x5488
00005314	e08f3003	add	r3, pc, r3
00005318	e58d3034	str	r3, [sp, #52]
0000531c	e28d2038	add	r2, sp, #56	; 0x38
00005320	e5827000	str	r7, [r2]
00005324	e59f3160	ldr	r3, [pc, #352]	; 0x548c
00005328	e08f3003	add	r3, pc, r3
0000532c	e5823004	str	r3, [r2, #4]
00005330	e582d008	str	sp, [r2, #8]
00005334	e28d3018	add	r3, sp, #24	; 0x18
00005338	e1a00003	mov	r0, r3
0000533c	eb0017cb	bl	0xb270	; symbol stub for: __Unwind_SjLj_Register
00005340	e59f3148	ldr	r3, [pc, #328]	; 0x5490
00005344	e08f3003	add	r3, pc, r3
00005348	e2832008	add	r2, r3, #8	; 0x8
0000534c	e59d304c	ldr	r3, [sp, #76]
00005350	e5832000	str	r2, [r3]
00005354	e59d304c	ldr	r3, [sp, #76]
00005358	e593200c	ldr	r2, [r3, #12]
0000535c	e59f3130	ldr	r3, [pc, #304]	; 0x5494
00005360	e08f3003	add	r3, pc, r3
00005364	e1520003	cmp	r2, r3
00005368	0a00000b	beq	0x539c
0000536c	e59d304c	ldr	r3, [sp, #76]
00005370	e593300c	ldr	r3, [r3, #12]
00005374	e58d3004	str	r3, [sp, #4]
00005378	e59d2004	ldr	r2, [sp, #4]
0000537c	e3520000	cmp	r2, #0	; 0x0
00005380	0a000005	beq	0x539c
00005384	e3a03001	mov	r3, #1	; 0x1
00005388	e58d301c	str	r3, [sp, #28]
0000538c	e59d0004	ldr	r0, [sp, #4]
00005390	eb0017e9	bl	0xb33c	; symbol stub for: __ZNSsD1Ev
00005394	e59d0004	ldr	r0, [sp, #4]
00005398	eb0017ea	bl	0xb348	; symbol stub for: __ZdlPv
0000539c	e59d304c	ldr	r3, [sp, #76]
000053a0	e2833004	add	r3, r3, #4	; 0x4
000053a4	e58d3008	str	r3, [sp, #8]
000053a8	e3a03002	mov	r3, #2	; 0x2
000053ac	e58d301c	str	r3, [sp, #28]
000053b0	e59d0008	ldr	r0, [sp, #8]
000053b4	eb0017c5	bl	0xb2d0	; symbol stub for: __ZN6google8protobuf15UnknownFieldSetD1Ev
000053b8	e59d304c	ldr	r3, [sp, #76]
000053bc	e58d300c	str	r3, [sp, #12]
000053c0	e3e03000	mvn	r3, #0	; 0x0
000053c4	e58d301c	str	r3, [sp, #28]
000053c8	e59d000c	ldr	r0, [sp, #12]
000053cc	eb0017c5	bl	0xb2e8	; symbol stub for: __ZN6google8protobuf7MessageD2Ev
000053d0	e3a03000	mov	r3, #0	; 0x0
000053d4	e6ef3073	uxtb r3,r3
000053d8	e3530000	cmp	r3, #0	; 0x0
000053dc	0a000020	beq	0x5464
000053e0	ea00001d	b	0x545c
000053e4	e59d301c	ldr	r3, [sp, #28]
000053e8	e59d2020	ldr	r2, [sp, #32]
000053ec	e58d2000	str	r2, [sp]
000053f0	e3530001	cmp	r3, #1	; 0x1
000053f4	0a00000a	beq	0x5424
000053f8	e59d3000	ldr	r3, [sp]
000053fc	e58d3010	str	r3, [sp, #16]
00005400	e59d304c	ldr	r3, [sp, #76]
00005404	e2833004	add	r3, r3, #4	; 0x4
00005408	e58d3008	str	r3, [sp, #8]
0000540c	e3a03000	mov	r3, #0	; 0x0
00005410	e58d301c	str	r3, [sp, #28]
00005414	e59d0008	ldr	r0, [sp, #8]
00005418	eb0017ac	bl	0xb2d0	; symbol stub for: __ZN6google8protobuf15UnknownFieldSetD1Ev
0000541c	e59d2010	ldr	r2, [sp, #16]
00005420	e58d2000	str	r2, [sp]
00005424	e59d3000	ldr	r3, [sp]
00005428	e58d3014	str	r3, [sp, #20]
0000542c	e59d204c	ldr	r2, [sp, #76]
00005430	e58d200c	str	r2, [sp, #12]
00005434	e3a03000	mov	r3, #0	; 0x0
00005438	e58d301c	str	r3, [sp, #28]
0000543c	e59d000c	ldr	r0, [sp, #12]
00005440	eb0017a8	bl	0xb2e8	; symbol stub for: __ZN6google8protobuf7MessageD2Ev
00005444	e59d3014	ldr	r3, [sp, #20]
00005448	e58d3000	str	r3, [sp]
0000544c	e3e03000	mvn	r3, #0	; 0x0
00005450	e58d301c	str	r3, [sp, #28]
00005454	e59d0000	ldr	r0, [sp]
00005458	eb001787	bl	0xb27c	; symbol stub for: __Unwind_SjLj_Resume
0000545c	e59d004c	ldr	r0, [sp, #76]
00005460	eb0017b8	bl	0xb348	; symbol stub for: __ZdlPv
00005464	e28d3018	add	r3, sp, #24	; 0x18
00005468	e1a00003	mov	r0, r3
0000546c	eb001785	bl	0xb288	; symbol stub for: __Unwind_SjLj_Unregister
00005470	e247d05c	sub	sp, r7, #92	; 0x5c
00005474	ecbd8b11	fldmiax	sp!, {d8-d15}
00005478	e247d018	sub	sp, r7, #24	; 0x18
0000547c	e8bd0d00	ldmia	sp!, {r8, r10, r11}
00005480	e8bd80f0	ldmia	sp!, {r4, r5, r6, r7, pc}
00005484	00006d14	andeq	r6, r0, r4, lsl sp
00005488	0000725c	andeq	r7, r0, ip, asr r2
0000548c	000000b4	streqh	r0, [r0], -r4
00005490	00006f74	andeq	r6, r0, r4, ror pc
00005494	0000737c	andeq	r7, r0, ip, ror r3
__ZN3dsp6HeaderD0Ev:
00005498	e92d40f0	stmdb	sp!, {r4, r5, r6, r7, lr}
0000549c	e28d700c	add	r7, sp, #12	; 0xc
000054a0	e92d0d00	stmdb	sp!, {r8, r10, r11}
000054a4	ed2d8b11	fstmdbx	sp!, {d8-d15}
000054a8	e24dd050	sub	sp, sp, #80	; 0x50
000054ac	e58d004c	str	r0, [sp, #76]
000054b0	e59f317c	ldr	r3, [pc, #380]	; 0x5634
000054b4	e08f3003	add	r3, pc, r3
000054b8	e5933000	ldr	r3, [r3]
000054bc	e58d3030	str	r3, [sp, #48]
000054c0	e59f3170	ldr	r3, [pc, #368]	; 0x5638
000054c4	e08f3003	add	r3, pc, r3
000054c8	e58d3034	str	r3, [sp, #52]
000054cc	e28d2038	add	r2, sp, #56	; 0x38
000054d0	e5827000	str	r7, [r2]
000054d4	e59f3160	ldr	r3, [pc, #352]	; 0x563c
000054d8	e08f3003	add	r3, pc, r3
000054dc	e5823004	str	r3, [r2, #4]
000054e0	e582d008	str	sp, [r2, #8]
000054e4	e28d3018	add	r3, sp, #24	; 0x18
000054e8	e1a00003	mov	r0, r3
000054ec	eb00175f	bl	0xb270	; symbol stub for: __Unwind_SjLj_Register
000054f0	e59f3148	ldr	r3, [pc, #328]	; 0x5640
000054f4	e08f3003	add	r3, pc, r3
000054f8	e2832008	add	r2, r3, #8	; 0x8
000054fc	e59d304c	ldr	r3, [sp, #76]
00005500	e5832000	str	r2, [r3]
00005504	e59d304c	ldr	r3, [sp, #76]
00005508	e593200c	ldr	r2, [r3, #12]
0000550c	e59f3130	ldr	r3, [pc, #304]	; 0x5644
00005510	e08f3003	add	r3, pc, r3
00005514	e1520003	cmp	r2, r3
00005518	0a00000b	beq	0x554c
0000551c	e59d304c	ldr	r3, [sp, #76]
00005520	e593300c	ldr	r3, [r3, #12]
00005524	e58d3004	str	r3, [sp, #4]
00005528	e59d2004	ldr	r2, [sp, #4]
0000552c	e3520000	cmp	r2, #0	; 0x0
00005530	0a000005	beq	0x554c
00005534	e3a03001	mov	r3, #1	; 0x1
00005538	e58d301c	str	r3, [sp, #28]
0000553c	e59d0004	ldr	r0, [sp, #4]
00005540	eb00177d	bl	0xb33c	; symbol stub for: __ZNSsD1Ev
00005544	e59d0004	ldr	r0, [sp, #4]
00005548	eb00177e	bl	0xb348	; symbol stub for: __ZdlPv
0000554c	e59d304c	ldr	r3, [sp, #76]
00005550	e2833004	add	r3, r3, #4	; 0x4
00005554	e58d3008	str	r3, [sp, #8]
00005558	e3a03002	mov	r3, #2	; 0x2
0000555c	e58d301c	str	r3, [sp, #28]
00005560	e59d0008	ldr	r0, [sp, #8]
00005564	eb001759	bl	0xb2d0	; symbol stub for: __ZN6google8protobuf15UnknownFieldSetD1Ev
00005568	e59d304c	ldr	r3, [sp, #76]
0000556c	e58d300c	str	r3, [sp, #12]
00005570	e3e03000	mvn	r3, #0	; 0x0
00005574	e58d301c	str	r3, [sp, #28]
00005578	e59d000c	ldr	r0, [sp, #12]
0000557c	eb001759	bl	0xb2e8	; symbol stub for: __ZN6google8protobuf7MessageD2Ev
00005580	e3a03001		mov	r3, #1	; 0x1
00005584	e6ef3073	uxtb r3,r3
00005588	e3530000	cmp	r3, #0	; 0x0
0000558c	0a000020	beq	0x5614
00005590	ea00001d	b	0x560c
00005594	e59d301c	ldr	r3, [sp, #28]
00005598	e59d2020	ldr	r2, [sp, #32]
0000559c	e58d2000	str	r2, [sp]
000055a0	e3530001	cmp	r3, #1	; 0x1
000055a4	0a00000a	beq	0x55d4
000055a8	e59d3000	ldr	r3, [sp]
000055ac	e58d3010	str	r3, [sp, #16]
000055b0	e59d304c	ldr	r3, [sp, #76]
000055b4	e2833004	add	r3, r3, #4	; 0x4
000055b8	e58d3008	str	r3, [sp, #8]
000055bc	e3a03000	mov	r3, #0	; 0x0
000055c0	e58d301c	str	r3, [sp, #28]
000055c4	e59d0008	ldr	r0, [sp, #8]
000055c8	eb001740	bl	0xb2d0	; symbol stub for: __ZN6google8protobuf15UnknownFieldSetD1Ev
000055cc	e59d2010	ldr	r2, [sp, #16]
000055d0	e58d2000	str	r2, [sp]
000055d4	e59d3000	ldr	r3, [sp]
000055d8	e58d3014	str	r3, [sp, #20]
000055dc	e59d204c	ldr	r2, [sp, #76]
000055e0	e58d200c	str	r2, [sp, #12]
000055e4	e3a03000	mov	r3, #0	; 0x0
000055e8	e58d301c	str	r3, [sp, #28]
000055ec	e59d000c	ldr	r0, [sp, #12]
000055f0	eb00173c	bl	0xb2e8	; symbol stub for: __ZN6google8protobuf7MessageD2Ev
000055f4	e59d3014	ldr	r3, [sp, #20]
000055f8	e58d3000	str	r3, [sp]
000055fc	e3e03000	mvn	r3, #0	; 0x0
00005600	e58d301c	str	r3, [sp, #28]
00005604	e59d0000	ldr	r0, [sp]
00005608	eb00171b	bl	0xb27c	; symbol stub for: __Unwind_SjLj_Resume
0000560c	e59d004c	ldr	r0, [sp, #76]
00005610	eb00174c	bl	0xb348	; symbol stub for: __ZdlPv
00005614	e28d3018	add	r3, sp, #24	; 0x18
00005618	e1a00003	mov	r0, r3
0000561c	eb001719	bl	0xb288	; symbol stub for: __Unwind_SjLj_Unregister
00005620	e247d05c	sub	sp, r7, #92	; 0x5c
00005624	ecbd8b11	fldmiax	sp!, {d8-d15}
00005628	e247d018	sub	sp, r7, #24	; 0x18
0000562c	e8bd0d00	ldmia	sp!, {r8, r10, r11}
00005630	e8bd80f0	ldmia	sp!, {r4, r5, r6, r7, pc}
00005634	00006b64	andeq	r6, r0, r4, ror #22
00005638	000070b4	streqh	r7, [r0], -r4
0000563c	000000b4	streqh	r0, [r0], -r4
00005640	00006dc4	andeq	r6, r0, r4, asr #27
00005644	000071cc	andeq	r7, r0, ip, asr #3
__ZN3dsp6Header10descriptorEv:
00005648	e92d4080	stmdb	sp!, {r7, lr}
0000564c	e28d7000	add	r7, sp, #0	; 0x0
00005650	e59f3024	ldr	r3, [pc, #36]	; 0x567c
00005654	e08f3003	add	r3, pc, r3
00005658	e5933000	ldr	r3, [r3]
0000565c	e3530000	cmp	r3, #0	; 0x0
00005660	1a000000	bne	0x5668
00005664	ebfff761	bl	__ZN3dsp30protobuf_BuildDesc_dsp_2eprotoEv
00005668	e59f3010	ldr	r3, [pc, #16]	; 0x5680
0000566c	e08f3003	add	r3, pc, r3
00005670	e5933000	ldr	r3, [r3]
00005674	e1a00003	mov	r0, r3
00005678	e8bd8080	ldmia	sp!, {r7, pc}
0000567c	00006ac4	andeq	r6, r0, r4, asr #21
00005680	00006aac	andeq	r6, r0, ip, lsr #21
__ZN3dsp6Header16default_instanceEv:
00005684	e92d4080	stmdb	sp!, {r7, lr}
00005688	e28d7000	add	r7, sp, #0	; 0x0
0000568c	e59f3024	ldr	r3, [pc, #36]	; 0x56b8
00005690	e08f3003	add	r3, pc, r3
00005694	e5933000	ldr	r3, [r3]
00005698	e3530000	cmp	r3, #0	; 0x0
0000569c	1a000000	bne	0x56a4
000056a0	ebfff752	bl	__ZN3dsp30protobuf_BuildDesc_dsp_2eprotoEv
000056a4	e59f3010	ldr	r3, [pc, #16]	; 0x56bc
000056a8	e08f3003	add	r3, pc, r3
000056ac	e5933000	ldr	r3, [r3]
000056b0	e1a00003	mov	r0, r3
000056b4	e8bd8080	ldmia	sp!, {r7, pc}
000056b8	00006a4c	andeq	r6, r0, ip, asr #20
000056bc	00006a34	andeq	r6, r0, r4, lsr r10
__ZNK3dsp6Header3NewEv:
000056c0	e92d40f0	stmdb	sp!, {r4, r5, r6, r7, lr}
000056c4	e28d700c	add	r7, sp, #12	; 0xc
000056c8	e92d0d00	stmdb	sp!, {r8, r10, r11}
000056cc	ed2d8b11	fstmdbx	sp!, {d8-d15}
000056d0	e24dd048	sub	sp, sp, #72	; 0x48
000056d4	e58d0044	str	r0, [sp, #68]
000056d8	e59f30c0	ldr	r3, [pc, #192]	; 0x57a0
000056dc	e08f3003	add	r3, pc, r3
000056e0	e5933000	ldr	r3, [r3]
000056e4	e58d3028	str	r3, [sp, #40]
000056e8	e59f30b4	ldr	r3, [pc, #180]	; 0x57a4
000056ec	e08f3003	add	r3, pc, r3
000056f0	e58d302c	str	r3, [sp, #44]
000056f4	e28d2030	add	r2, sp, #48	; 0x30
000056f8	e5827000	str	r7, [r2]
000056fc	e59f30a4	ldr	r3, [pc, #164]	; 0x57a8
00005700	e08f3003	add	r3, pc, r3
00005704	e5823004	str	r3, [r2, #4]
00005708	e582d008	str	sp, [r2, #8]
0000570c	e28d3010	add	r3, sp, #16	; 0x10
00005710	e1a00003	mov	r0, r3
00005714	eb0016d5	bl	0xb270	; symbol stub for: __Unwind_SjLj_Register
00005718	e3e03000	mvn	r3, #0	; 0x0
0000571c	e58d3014	str	r3, [sp, #20]
00005720	e3a00024	mov	r0, #36	; 0x24
00005724	eb00170a	bl	0xb354	; symbol stub for: __Znwm
00005728	e1a03000	mov	r3, r0
0000572c	e58d3008	str	r3, [sp, #8]
00005730	e3a03001	mov	r3, #1	; 0x1
00005734	e58d3014	str	r3, [sp, #20]
00005738	e59d0008	ldr	r0, [sp, #8]
0000573c	ebfff97e	bl	__ZN3dsp6HeaderC1Ev
00005740	ea00000b	b	0x5774
00005744	e59d3018	ldr	r3, [sp, #24]
00005748	e58d3000	str	r3, [sp]
0000574c	e59d3000	ldr	r3, [sp]
00005750	e58d300c	str	r3, [sp, #12]
00005754	e59d0008	ldr	r0, [sp, #8]
00005758	eb0016fa	bl	0xb348	; symbol stub for: __ZdlPv
0000575c	e59d300c	ldr	r3, [sp, #12]
00005760	e58d3000	str	r3, [sp]
00005764	e3e03000	mvn	r3, #0	; 0x0
00005768	e58d3014	str	r3, [sp, #20]
0000576c	e59d0000	ldr	r0, [sp]
00005770	eb0016c1	bl	0xb27c	; symbol stub for: __Unwind_SjLj_Resume
00005774	e59d3008	ldr	r3, [sp, #8]
00005778	e58d3004	str	r3, [sp, #4]
0000577c	e28d3010	add	r3, sp, #16	; 0x10
00005780	e1a00003	mov	r0, r3
00005784	eb0016bf	bl	0xb288	; symbol stub for: __Unwind_SjLj_Unregister
00005788	e59d0004	ldr	r0, [sp, #4]
0000578c	e247d05c	sub	sp, r7, #92	; 0x5c
00005790	ecbd8b11	fldmiax	sp!, {d8-d15}
00005794	e247d018	sub	sp, r7, #24	; 0x18
00005798	e8bd0d00	ldmia	sp!, {r8, r10, r11}
0000579c	e8bd80f0	ldmia	sp!, {r4, r5, r6, r7, pc}
000057a0	0000693c	andeq	r6, r0, ip, lsr r9
000057a4	00006e94	muleq	r0, r4, lr
000057a8	0000003c	andeq	r0, r0, ip, lsr r0
__ZNK3dsp6Header13GetDescriptorEv:
000057ac	e92d4080	stmdb	sp!, {r7, lr}
000057b0	e28d7000	add	r7, sp, #0	; 0x0
000057b4	e24dd004	sub	sp, sp, #4	; 0x4
000057b8	e58d0000	str	r0, [sp]
000057bc	ebffffa1	bl	__ZN3dsp6Header10descriptorEv
000057c0	e1a03000	mov	r3, r0
000057c4	e1a00003	mov	r0, r3
000057c8	e247d000	sub	sp, r7, #0	; 0x0
000057cc	e8bd8080	ldmia	sp!, {r7, pc}
__ZNK3dsp6Header13GetReflectionEv:
000057d0	e92d4080	stmdb	sp!, {r7, lr}
000057d4	e28d7000	add	r7, sp, #0	; 0x0
000057d8	e24dd004	sub	sp, sp, #4	; 0x4
000057dc	e58d0000	str	r0, [sp]
000057e0	e59f3028	ldr	r3, [pc, #40]	; 0x5810
000057e4	e08f3003	add	r3, pc, r3
000057e8	e5933000	ldr	r3, [r3]
000057ec	e3530000	cmp	r3, #0	; 0x0
000057f0	1a000000	bne	0x57f8
000057f4	ebfff6fd	bl	__ZN3dsp30protobuf_BuildDesc_dsp_2eprotoEv
000057f8	e59f3014	ldr	r3, [pc, #20]	; 0x5814
000057fc	e08f3003	add	r3, pc, r3
00005800	e5933000	ldr	r3, [r3]
00005804	e1a00003	mov	r0, r3
00005808	e247d000	sub	sp, r7, #0	; 0x0
0000580c	e8bd8080	ldmia	sp!, {r7, pc}
00005810	00006930	andeq	r6, r0, r0, lsr r9
00005814	00006918	andeq	r6, r0, r8, lsl r9
__ZN3dsp24AccelerometerDescriptionC2Ev:
00005818	e92d40f0	stmdb	sp!, {r4, r5, r6, r7, lr}
0000581c	e28d700c	add	r7, sp, #12	; 0xc
00005820	e92d0d00	stmdb	sp!, {r8, r10, r11}
00005824	ed2d8b11	fstmdbx	sp!, {d8-d15}
00005828	e24dd040	sub	sp, sp, #64	; 0x40
0000582c	e58d003c	str	r0, [sp, #60]
00005830	e59f3104	ldr	r3, [pc, #260]	; 0x593c
00005834	e08f3003	add	r3, pc, r3
00005838	e5933000	ldr	r3, [r3]
0000583c	e58d3020	str	r3, [sp, #32]
00005840	e59f30f8	ldr	r3, [pc, #248]	; 0x5940
00005844	e08f3003	add	r3, pc, r3
00005848	e58d3024	str	r3, [sp, #36]
0000584c	e28d2028	add	r2, sp, #40	; 0x28
00005850	e5827000	str	r7, [r2]
00005854	e59f30e8	ldr	r3, [pc, #232]	; 0x5944
00005858	e08f3003	add	r3, pc, r3
0000585c	e5823004	str	r3, [r2, #4]
00005860	e582d008	str	sp, [r2, #8]
00005864	e28d3008	add	r3, sp, #8	; 0x8
00005868	e1a00003	mov	r0, r3
0000586c	eb00167f	bl	0xb270	; symbol stub for: __Unwind_SjLj_Register
00005870	e59d303c	ldr	r3, [sp, #60]
00005874	e1a00003	mov	r0, r3
00005878	e59f30c8	ldr	r3, [pc, #200]	; 0x5948
0000587c	e08f3003	add	r3, pc, r3
00005880	e5933000	ldr	r3, [r3]
00005884	e12fff33	blx	r3
00005888	e59f30bc	ldr	r3, [pc, #188]	; 0x594c
0000588c	e08f3003	add	r3, pc, r3
00005890	e2832008	add	r2, r3, #8	; 0x8
00005894	e59d303c	ldr	r3, [sp, #60]
00005898	e5832000	str	r2, [r3]
0000589c	e59d303c	ldr	r3, [sp, #60]
000058a0	e2832004	add	r2, r3, #4	; 0x4
000058a4	e3a03001	mov	r3, #1	; 0x1
000058a8	e58d300c	str	r3, [sp, #12]
000058ac	e1a00002	mov	r0, r2
000058b0	eb001683	bl	0xb2c4	; symbol stub for: __ZN6google8protobuf15UnknownFieldSetC1Ev
000058b4	e59d203c	ldr	r2, [sp, #60]
000058b8	e3a03000	mov	r3, #0	; 0x0
000058bc	e5823008	str	r3, [r2, #8]
000058c0	e59d203c	ldr	r2, [sp, #60]
000058c4	e59f3084	ldr	r3, [pc, #132]	; 0x5950
000058c8	e582300c	str	r3, [r2, #12]
000058cc	e59d303c	ldr	r3, [sp, #60]
000058d0	e2832010	add	r2, r3, #16	; 0x10
000058d4	e3a03000	mov	r3, #0	; 0x0
000058d8	e5823000	str	r3, [r2]
000058dc	ea00000e	b	0x591c
000058e0	e59d3010	ldr	r3, [sp, #16]
000058e4	e58d3000	str	r3, [sp]
000058e8	e59d3000	ldr	r3, [sp]
000058ec	e58d3004	str	r3, [sp, #4]
000058f0	e59d203c	ldr	r2, [sp, #60]
000058f4	e3a03000	mov	r3, #0	; 0x0
000058f8	e58d300c	str	r3, [sp, #12]
000058fc	e1a00002	mov	r0, r2
00005900	eb001678	bl	0xb2e8	; symbol stub for: __ZN6google8protobuf7MessageD2Ev
00005904	e59d3004	ldr	r3, [sp, #4]
00005908	e58d3000	str	r3, [sp]
0000590c	e3e03000	mvn	r3, #0	; 0x0
00005910	e58d300c	str	r3, [sp, #12]
00005914	e59d0000	ldr	r0, [sp]
00005918	eb001657	bl	0xb27c	; symbol stub for: __Unwind_SjLj_Resume
0000591c	e28d3008	add	r3, sp, #8	; 0x8
00005920	e1a00003	mov	r0, r3
00005924	eb001657	bl	0xb288	; symbol stub for: __Unwind_SjLj_Unregister
00005928	e247d05c	sub	sp, r7, #92	; 0x5c
0000592c	ecbd8b11	fldmiax	sp!, {d8-d15}
00005930	e247d018	sub	sp, r7, #24	; 0x18
00005934	e8bd0d00	ldmia	sp!, {r8, r10, r11}
00005938	e8bd80f0	ldmia	sp!, {r4, r5, r6, r7, pc}
0000593c	000067e4	andeq	r6, r0, r4, ror #15
00005940	00006d42	andeq	r6, r0, r2, asr #26
00005944	00000080	andeq	r0, r0, r0, lsl #1
00005948	000067a4	andeq	r6, r0, r4, lsr #15
0000594c	00006a74	andeq	r6, r0, r4, ror r10
00005950	00000000	andeq	r0, r0, r0
__ZN3dsp24AccelerometerDescriptionC2ERKS0_:
00005954	e92d40f0	stmdb	sp!, {r4, r5, r6, r7, lr}
00005958	e28d700c	add	r7, sp, #12	; 0xc
0000595c	e92d0d00	stmdb	sp!, {r8, r10, r11}
00005960	ed2d8b11	fstmdbx	sp!, {d8-d15}
00005964	e24dd048	sub	sp, sp, #72	; 0x48
00005968	e58d0044	str	r0, [sp, #68]
0000596c	e58d1040	str	r1, [sp, #64]
00005970	e59f3150	ldr	r3, [pc, #336]	; 0x5ac8
00005974	e08f3003	add	r3, pc, r3
00005978	e5933000	ldr	r3, [r3]
0000597c	e58d3024	str	r3, [sp, #36]
00005980	e59f3144	ldr	r3, [pc, #324]	; 0x5acc
00005984	e08f3003	add	r3, pc, r3
00005988	e58d3028	str	r3, [sp, #40]
0000598c	e28d202c	add	r2, sp, #44	; 0x2c
00005990	e5827000	str	r7, [r2]
00005994	e59f3134	ldr	r3, [pc, #308]	; 0x5ad0
00005998	e08f3003	add	r3, pc, r3
0000599c	e5823004	str	r3, [r2, #4]
000059a0	e582d008	str	sp, [r2, #8]
000059a4	e28d300c	add	r3, sp, #12	; 0xc
000059a8	e1a00003	mov	r0, r3
000059ac	eb00162f	bl	0xb270	; symbol stub for: __Unwind_SjLj_Register
000059b0	e59d3044	ldr	r3, [sp, #68]
000059b4	e1a00003	mov	r0, r3
000059b8	e59f3114	ldr	r3, [pc, #276]	; 0x5ad4
000059bc	e08f3003	add	r3, pc, r3
000059c0	e5933000	ldr	r3, [r3]
000059c4	e12fff33	blx	r3
000059c8	e59f3108	ldr	r3, [pc, #264]	; 0x5ad8
000059cc	e08f3003	add	r3, pc, r3
000059d0	e2832008	add	r2, r3, #8	; 0x8
000059d4	e59d3044	ldr	r3, [sp, #68]
000059d8	e5832000	str	r2, [r3]
000059dc	e59d3044	ldr	r3, [sp, #68]
000059e0	e2832004	add	r2, r3, #4	; 0x4
000059e4	e3a03002	mov	r3, #2	; 0x2
000059e8	e58d3010	str	r3, [sp, #16]
000059ec	e1a00002	mov	r0, r2
000059f0	eb001633	bl	0xb2c4	; symbol stub for: __ZN6google8protobuf15UnknownFieldSetC1Ev
000059f4	e59d2044	ldr	r2, [sp, #68]
000059f8	e3a03000	mov	r3, #0	; 0x0
000059fc	e5823008	str	r3, [r2, #8]
00005a00	e59d2044	ldr	r2, [sp, #68]
00005a04	e59f30d0	ldr	r3, [pc, #208]	; 0x5adc
00005a08	e582300c	str	r3, [r2, #12]
00005a0c	e59d3044	ldr	r3, [sp, #68]
00005a10	e2832010	add	r2, r3, #16	; 0x10
00005a14	e3a03000	mov	r3, #0	; 0x0
00005a18	e5823000	str	r3, [r2]
00005a1c	e59d2044	ldr	r2, [sp, #68]
00005a20	e59d1040	ldr	r1, [sp, #64]
00005a24	e3a03001	mov	r3, #1	; 0x1
00005a28	e58d3010	str	r3, [sp, #16]
00005a2c	e1a00002	mov	r0, r2
00005a30	eb001629	bl	0xb2dc	; symbol stub for: __ZN6google8protobuf7Message9MergeFromERKS1_
00005a34	ea00001b	b	0x5aa8
00005a38	e59d3010	ldr	r3, [sp, #16]
00005a3c	e59d2014	ldr	r2, [sp, #20]
00005a40	e58d2000	str	r2, [sp]
00005a44	e3530001	cmp	r3, #1	; 0x1
00005a48	0a000009	beq	0x5a74
00005a4c	e59d3000	ldr	r3, [sp]
00005a50	e58d3004	str	r3, [sp, #4]
00005a54	e59d3044	ldr	r3, [sp, #68]
00005a58	e2832004	add	r2, r3, #4	; 0x4
00005a5c	e3a03000	mov	r3, #0	; 0x0
00005a60	e58d3010	str	r3, [sp, #16]
00005a64	e1a00002	mov	r0, r2
00005a68	eb001618	bl	0xb2d0	; symbol stub for: __ZN6google8protobuf15UnknownFieldSetD1Ev
00005a6c	e59d2004	ldr	r2, [sp, #4]
00005a70	e58d2000	str	r2, [sp]
00005a74	e59d3000	ldr	r3, [sp]
00005a78	e58d3008	str	r3, [sp, #8]
00005a7c	e59d2044	ldr	r2, [sp, #68]
00005a80	e3a03000	mov	r3, #0	; 0x0
00005a84	e58d3010	str	r3, [sp, #16]
00005a88	e1a00002	mov	r0, r2
00005a8c	eb001615	bl	0xb2e8	; symbol stub for: __ZN6google8protobuf7MessageD2Ev
00005a90	e59d2008	ldr	r2, [sp, #8]
00005a94	e58d2000	str	r2, [sp]
00005a98	e3e03000	mvn	r3, #0	; 0x0
00005a9c	e58d3010	str	r3, [sp, #16]
00005aa0	e59d0000	ldr	r0, [sp]
00005aa4	eb0015f4	bl	0xb27c	; symbol stub for: __Unwind_SjLj_Resume
00005aa8	e28d300c	add	r3, sp, #12	; 0xc
00005aac	e1a00003	mov	r0, r3
00005ab0	eb0015f4	bl	0xb288	; symbol stub for: __Unwind_SjLj_Unregister
00005ab4	e247d05c	sub	sp, r7, #92	; 0x5c
00005ab8	ecbd8b11	fldmiax	sp!, {d8-d15}
00005abc	e247d018	sub	sp, r7, #24	; 0x18
00005ac0	e8bd0d00	ldmia	sp!, {r8, r10, r11}
00005ac4	e8bd80f0	ldmia	sp!, {r4, r5, r6, r7, pc}
00005ac8	000066a4	andeq	r6, r0, r4, lsr #13
00005acc	00006c08	andeq	r6, r0, r8, lsl #24
00005ad0	00000098	muleq	r0, r8, r0
00005ad4	00006664	andeq	r6, r0, r4, ror #12
00005ad8	00006934	andeq	r6, r0, r4, lsr r9
00005adc	00000000	andeq	r0, r0, r0
__ZN3dsp24AccelerometerDescriptionC1ERKS0_:
00005ae0	e92d40f0	stmdb	sp!, {r4, r5, r6, r7, lr}
00005ae4	e28d700c	add	r7, sp, #12	; 0xc
00005ae8	e92d0d00	stmdb	sp!, {r8, r10, r11}
00005aec	ed2d8b11	fstmdbx	sp!, {d8-d15}
00005af0	e24dd048	sub	sp, sp, #72	; 0x48
00005af4	e58d0044	str	r0, [sp, #68]
00005af8	e58d1040	str	r1, [sp, #64]
00005afc	e59f3150	ldr	r3, [pc, #336]	; 0x5c54
00005b00	e08f3003	add	r3, pc, r3
00005b04	e5933000	ldr	r3, [r3]
00005b08	e58d3024	str	r3, [sp, #36]
00005b0c	e59f3144	ldr	r3, [pc, #324]	; 0x5c58
00005b10	e08f3003	add	r3, pc, r3
00005b14	e58d3028	str	r3, [sp, #40]
00005b18	e28d202c	add	r2, sp, #44	; 0x2c
00005b1c	e5827000	str	r7, [r2]
00005b20	e59f3134	ldr	r3, [pc, #308]	; 0x5c5c
00005b24	e08f3003	add	r3, pc, r3
00005b28	e5823004	str	r3, [r2, #4]
00005b2c	e582d008	str	sp, [r2, #8]
00005b30	e28d300c	add	r3, sp, #12	; 0xc
00005b34	e1a00003	mov	r0, r3
00005b38	eb0015cc	bl	0xb270	; symbol stub for: __Unwind_SjLj_Register
00005b3c	e59d3044	ldr	r3, [sp, #68]
00005b40	e1a00003	mov	r0, r3
00005b44	e59f3114	ldr	r3, [pc, #276]	; 0x5c60
00005b48	e08f3003	add	r3, pc, r3
00005b4c	e5933000	ldr	r3, [r3]
00005b50	e12fff33	blx	r3
00005b54	e59f3108	ldr	r3, [pc, #264]	; 0x5c64
00005b58	e08f3003	add	r3, pc, r3
00005b5c	e2832008	add	r2, r3, #8	; 0x8
00005b60	e59d3044	ldr	r3, [sp, #68]
00005b64	e5832000	str	r2, [r3]
00005b68	e59d3044	ldr	r3, [sp, #68]
00005b6c	e2832004	add	r2, r3, #4	; 0x4
00005b70	e3a03002	mov	r3, #2	; 0x2
00005b74	e58d3010	str	r3, [sp, #16]
00005b78	e1a00002	mov	r0, r2
00005b7c	eb0015d0	bl	0xb2c4	; symbol stub for: __ZN6google8protobuf15UnknownFieldSetC1Ev
00005b80	e59d2044	ldr	r2, [sp, #68]
00005b84	e3a03000	mov	r3, #0	; 0x0
00005b88	e5823008	str	r3, [r2, #8]
00005b8c	e59d2044	ldr	r2, [sp, #68]
00005b90	e59f30d0	ldr	r3, [pc, #208]	; 0x5c68
00005b94	e582300c	str	r3, [r2, #12]
00005b98	e59d3044	ldr	r3, [sp, #68]
00005b9c	e2832010	add	r2, r3, #16	; 0x10
00005ba0	e3a03000	mov	r3, #0	; 0x0
00005ba4	e5823000	str	r3, [r2]
00005ba8	e59d2044	ldr	r2, [sp, #68]
00005bac	e59d1040	ldr	r1, [sp, #64]
00005bb0	e3a03001	mov	r3, #1	; 0x1
00005bb4	e58d3010	str	r3, [sp, #16]
00005bb8	e1a00002	mov	r0, r2
00005bbc	eb0015c6	bl	0xb2dc	; symbol stub for: __ZN6google8protobuf7Message9MergeFromERKS1_
00005bc0	ea00001b	b	0x5c34
00005bc4	e59d3010	ldr	r3, [sp, #16]
00005bc8	e59d2014	ldr	r2, [sp, #20]
00005bcc	e58d2000	str	r2, [sp]
00005bd0	e3530001	cmp	r3, #1	; 0x1
00005bd4	0a000009	beq	0x5c00
00005bd8	e59d3000	ldr	r3, [sp]
00005bdc	e58d3004	str	r3, [sp, #4]
00005be0	e59d3044	ldr	r3, [sp, #68]
00005be4	e2832004	add	r2, r3, #4	; 0x4
00005be8	e3a03000	mov	r3, #0	; 0x0
00005bec	e58d3010	str	r3, [sp, #16]
00005bf0	e1a00002	mov	r0, r2
00005bf4	eb0015b5	bl	0xb2d0	; symbol stub for: __ZN6google8protobuf15UnknownFieldSetD1Ev
00005bf8	e59d2004	ldr	r2, [sp, #4]
00005bfc	e58d2000	str	r2, [sp]
00005c00	e59d3000	ldr	r3, [sp]
00005c04	e58d3008	str	r3, [sp, #8]
00005c08	e59d2044	ldr	r2, [sp, #68]
00005c0c	e3a03000	mov	r3, #0	; 0x0
00005c10	e58d3010	str	r3, [sp, #16]
00005c14	e1a00002	mov	r0, r2
00005c18	eb0015b2	bl	0xb2e8	; symbol stub for: __ZN6google8protobuf7MessageD2Ev
00005c1c	e59d2008	ldr	r2, [sp, #8]
00005c20	e58d2000	str	r2, [sp]
00005c24	e3e03000	mvn	r3, #0	; 0x0
00005c28	e58d3010	str	r3, [sp, #16]
00005c2c	e59d0000	ldr	r0, [sp]
00005c30	eb001591	bl	0xb27c	; symbol stub for: __Unwind_SjLj_Resume
00005c34	e28d300c	add	r3, sp, #12	; 0xc
00005c38	e1a00003	mov	r0, r3
00005c3c	eb001591	bl	0xb288	; symbol stub for: __Unwind_SjLj_Unregister
00005c40	e247d05c	sub	sp, r7, #92	; 0x5c
00005c44	ecbd8b11	fldmiax	sp!, {d8-d15}
00005c48	e247d018	sub	sp, r7, #24	; 0x18
00005c4c	e8bd0d00	ldmia	sp!, {r8, r10, r11}
00005c50	e8bd80f0	ldmia	sp!, {r4, r5, r6, r7, pc}
00005c54	00006518	andeq	r6, r0, r8, lsl r5
00005c58	00006a84	andeq	r6, r0, r4, lsl #21
00005c5c	00000098	muleq	r0, r8, r0
00005c60	000064d8	ldreqd	r6, [r0], -r8
00005c64	000067a8	andeq	r6, r0, r8, lsr #15
00005c68	00000000	andeq	r0, r0, r0
__ZN3dsp24AccelerometerDescriptionD2Ev:
00005c6c	e92d40f0	stmdb	sp!, {r4, r5, r6, r7, lr}
00005c70	e28d700c	add	r7, sp, #12	; 0xc
00005c74	e92d0d00	stmdb	sp!, {r8, r10, r11}
00005c78	ed2d8b11	fstmdbx	sp!, {d8-d15}
00005c7c	e24dd044	sub	sp, sp, #68	; 0x44
00005c80	e58d0040	str	r0, [sp, #64]
00005c84	e59f30f8	ldr	r3, [pc, #248]	; 0x5d84
00005c88	e08f3003	add	r3, pc, r3
00005c8c	e5933000	ldr	r3, [r3]
00005c90	e58d3024	str	r3, [sp, #36]
00005c94	e59f30ec	ldr	r3, [pc, #236]	; 0x5d88
00005c98	e08f3003	add	r3, pc, r3
00005c9c	e58d3028	str	r3, [sp, #40]
00005ca0	e28d202c	add	r2, sp, #44	; 0x2c
00005ca4	e5827000	str	r7, [r2]
00005ca8	e59f30dc	ldr	r3, [pc, #220]	; 0x5d8c
00005cac	e08f3003	add	r3, pc, r3
00005cb0	e5823004	str	r3, [r2, #4]
00005cb4	e582d008	str	sp, [r2, #8]
00005cb8	e28d300c	add	r3, sp, #12	; 0xc
00005cbc	e1a00003	mov	r0, r3
00005cc0	eb00156a	bl	0xb270	; symbol stub for: __Unwind_SjLj_Register
00005cc4	e59f30c4	ldr	r3, [pc, #196]	; 0x5d90
00005cc8	e08f3003	add	r3, pc, r3
00005ccc	e2832008	add	r2, r3, #8	; 0x8
00005cd0	e59d3040	ldr	r3, [sp, #64]
00005cd4	e5832000	str	r2, [r3]
00005cd8	e59d3040	ldr	r3, [sp, #64]
00005cdc	e2832004	add	r2, r3, #4	; 0x4
00005ce0	e3a03001	mov	r3, #1	; 0x1
00005ce4	e58d3010	str	r3, [sp, #16]
00005ce8	e1a00002	mov	r0, r2
00005cec	eb001577	bl	0xb2d0	; symbol stub for: __ZN6google8protobuf15UnknownFieldSetD1Ev
00005cf0	e59d3040	ldr	r3, [sp, #64]
00005cf4	e58d3004	str	r3, [sp, #4]
00005cf8	e3e03000	mvn	r3, #0	; 0x0
00005cfc	e58d3010	str	r3, [sp, #16]
00005d00	e59d0004	ldr	r0, [sp, #4]
00005d04	eb001577	bl	0xb2e8	; symbol stub for: __ZN6google8protobuf7MessageD2Ev
00005d08	e3a03000	mov	r3, #0	; 0x0
00005d0c	e6ef3073	uxtb r3,r3
00005d10	e3530000	cmp	r3, #0	; 0x0
00005d14	0a000012	beq	0x5d64
00005d18	ea00000f	b	0x5d5c
00005d1c	e59d3014	ldr	r3, [sp, #20]
00005d20	e58d3000	str	r3, [sp]
00005d24	e59d3000	ldr	r3, [sp]
00005d28	e58d3008	str	r3, [sp, #8]
00005d2c	e59d3040	ldr	r3, [sp, #64]
00005d30	e58d3004	str	r3, [sp, #4]
00005d34	e3a03000	mov	r3, #0	; 0x0
00005d38	e58d3010	str	r3, [sp, #16]
00005d3c	e59d0004	ldr	r0, [sp, #4]
00005d40	eb001568	bl	0xb2e8	; symbol stub for: __ZN6google8protobuf7MessageD2Ev
00005d44	e59d3008	ldr	r3, [sp, #8]
00005d48	e58d3000	str	r3, [sp]
00005d4c	e3e03000	mvn	r3, #0	; 0x0
00005d50	e58d3010	str	r3, [sp, #16]
00005d54	e59d0000	ldr	r0, [sp]
00005d58	eb001547	bl	0xb27c	; symbol stub for: __Unwind_SjLj_Resume
00005d5c	e59d0040	ldr	r0, [sp, #64]
00005d60	eb001578	bl	0xb348	; symbol stub for: __ZdlPv
00005d64	e28d300c	add	r3, sp, #12	; 0xc
00005d68	e1a00003	mov	r0, r3
00005d6c	eb001545	bl	0xb288	; symbol stub for: __Unwind_SjLj_Unregister
00005d70	e247d05c	sub	sp, r7, #92	; 0x5c
00005d74	ecbd8b11	fldmiax	sp!, {d8-d15}
00005d78	e247d018	sub	sp, r7, #24	; 0x18
00005d7c	e8bd0d00	ldmia	sp!, {r8, r10, r11}
00005d80	e8bd80f0	ldmia	sp!, {r4, r5, r6, r7, pc}
00005d84	00006390	muleq	r0, r0, r3
00005d88	00006904	andeq	r6, r0, r4, lsl #18
00005d8c	00000068	andeq	r0, r0, r8, rrx
00005d90	00006638	andeq	r6, r0, r8, lsr r6
__ZN3dsp24AccelerometerDescriptionD1Ev:
00005d94	e92d40f0	stmdb	sp!, {r4, r5, r6, r7, lr}
00005d98	e28d700c	add	r7, sp, #12	; 0xc
00005d9c	e92d0d00	stmdb	sp!, {r8, r10, r11}
00005da0	ed2d8b11	fstmdbx	sp!, {d8-d15}
00005da4	e24dd044	sub	sp, sp, #68	; 0x44
00005da8	e58d0040	str	r0, [sp, #64]
00005dac	e59f30f8	ldr	r3, [pc, #248]	; 0x5eac
00005db0	e08f3003	add	r3, pc, r3
00005db4	e5933000	ldr	r3, [r3]
00005db8	e58d3024	str	r3, [sp, #36]
00005dbc	e59f30ec	ldr	r3, [pc, #236]	; 0x5eb0
00005dc0	e08f3003	add	r3, pc, r3
00005dc4	e58d3028	str	r3, [sp, #40]
00005dc8	e28d202c	add	r2, sp, #44	; 0x2c
00005dcc	e5827000	str	r7, [r2]
00005dd0	e59f30dc	ldr	r3, [pc, #220]	; 0x5eb4
00005dd4	e08f3003	add	r3, pc, r3
00005dd8	e5823004	str	r3, [r2, #4]
00005ddc	e582d008	str	sp, [r2, #8]
00005de0	e28d300c	add	r3, sp, #12	; 0xc
00005de4	e1a00003	mov	r0, r3
00005de8	eb001520	bl	0xb270	; symbol stub for: __Unwind_SjLj_Register
00005dec	e59f30c4	ldr	r3, [pc, #196]	; 0x5eb8
00005df0	e08f3003	add	r3, pc, r3
00005df4	e2832008	add	r2, r3, #8	; 0x8
00005df8	e59d3040	ldr	r3, [sp, #64]
00005dfc	e5832000	str	r2, [r3]
00005e00	e59d3040	ldr	r3, [sp, #64]
00005e04	e2832004	add	r2, r3, #4	; 0x4
00005e08	e3a03001	mov	r3, #1	; 0x1
00005e0c	e58d3010	str	r3, [sp, #16]
00005e10	e1a00002	mov	r0, r2
00005e14	eb00152d	bl	0xb2d0	; symbol stub for: __ZN6google8protobuf15UnknownFieldSetD1Ev
00005e18	e59d3040	ldr	r3, [sp, #64]
00005e1c	e58d3004	str	r3, [sp, #4]
00005e20	e3e03000	mvn	r3, #0	; 0x0
00005e24	e58d3010	str	r3, [sp, #16]
00005e28	e59d0004	ldr	r0, [sp, #4]
00005e2c	eb00152d	bl	0xb2e8	; symbol stub for: __ZN6google8protobuf7MessageD2Ev
00005e30	e3a03000	mov	r3, #0	; 0x0
00005e34	e6ef3073	uxtb r3,r3
00005e38	e3530000	cmp	r3, #0	; 0x0
00005e3c	0a000012	beq	0x5e8c
00005e40	ea00000f	b	0x5e84
00005e44	e59d3014	ldr	r3, [sp, #20]
00005e48	e58d3000	str	r3, [sp]
00005e4c	e59d3000	ldr	r3, [sp]
00005e50	e58d3008	str	r3, [sp, #8]
00005e54	e59d3040	ldr	r3, [sp, #64]
00005e58	e58d3004	str	r3, [sp, #4]
00005e5c	e3a03000	mov	r3, #0	; 0x0
00005e60	e58d3010	str	r3, [sp, #16]
00005e64	e59d0004	ldr	r0, [sp, #4]
00005e68	eb00151e	bl	0xb2e8	; symbol stub for: __ZN6google8protobuf7MessageD2Ev
00005e6c	e59d3008	ldr	r3, [sp, #8]
00005e70	e58d3000	str	r3, [sp]
00005e74	e3e03000	mvn	r3, #0	; 0x0
00005e78	e58d3010	str	r3, [sp, #16]
00005e7c	e59d0000	ldr	r0, [sp]
00005e80	eb0014fd	bl	0xb27c	; symbol stub for: __Unwind_SjLj_Resume
00005e84	e59d0040	ldr	r0, [sp, #64]
00005e88	eb00152e	bl	0xb348	; symbol stub for: __ZdlPv
00005e8c	e28d300c	add	r3, sp, #12	; 0xc
00005e90	e1a00003	mov	r0, r3
00005e94	eb0014fb	bl	0xb288	; symbol stub for: __Unwind_SjLj_Unregister
00005e98	e247d05c	sub	sp, r7, #92	; 0x5c
00005e9c	ecbd8b11	fldmiax	sp!, {d8-d15}
00005ea0	e247d018	sub	sp, r7, #24	; 0x18
00005ea4	e8bd0d00	ldmia	sp!, {r8, r10, r11}
00005ea8	e8bd80f0	ldmia	sp!, {r4, r5, r6, r7, pc}
00005eac	00006268	andeq	r6, r0, r8, ror #4
00005eb0	000067e2	andeq	r6, r0, r2, ror #15
00005eb4	00000068	andeq	r0, r0, r8, rrx
00005eb8	00006510	andeq	r6, r0, r0, lsl r5
__ZN3dsp24AccelerometerDescriptionD0Ev:
00005ebc	e92d40f0	stmdb	sp!, {r4, r5, r6, r7, lr}
00005ec0	e28d700c	add	r7, sp, #12	; 0xc
00005ec4	e92d0d00	stmdb	sp!, {r8, r10, r11}
00005ec8	ed2d8b11	fstmdbx	sp!, {d8-d15}
00005ecc	e24dd044	sub	sp, sp, #68	; 0x44
00005ed0	e58d0040	str	r0, [sp, #64]
00005ed4	e59f30f8	ldr	r3, [pc, #248]	; 0x5fd4
00005ed8	e08f3003	add	r3, pc, r3
00005edc	e5933000	ldr	r3, [r3]
00005ee0	e58d3024	str	r3, [sp, #36]
00005ee4	e59f30ec	ldr	r3, [pc, #236]	; 0x5fd8
00005ee8	e08f3003	add	r3, pc, r3
00005eec	e58d3028	str	r3, [sp, #40]
00005ef0	e28d202c	add	r2, sp, #44	; 0x2c
00005ef4	e5827000	str	r7, [r2]
00005ef8	e59f30dc	ldr	r3, [pc, #220]	; 0x5fdc
00005efc	e08f3003	add	r3, pc, r3
00005f00	e5823004	str	r3, [r2, #4]
00005f04	e582d008	str	sp, [r2, #8]
00005f08	e28d300c	add	r3, sp, #12	; 0xc
00005f0c	e1a00003	mov	r0, r3
00005f10	eb0014d6	bl	0xb270	; symbol stub for: __Unwind_SjLj_Register
00005f14	e59f30c4	ldr	r3, [pc, #196]	; 0x5fe0
00005f18	e08f3003	add	r3, pc, r3
00005f1c	e2832008	add	r2, r3, #8	; 0x8
00005f20	e59d3040	ldr	r3, [sp, #64]
00005f24	e5832000	str	r2, [r3]
00005f28	e59d3040	ldr	r3, [sp, #64]
00005f2c	e2832004	add	r2, r3, #4	; 0x4
00005f30	e3a03001	mov	r3, #1	; 0x1
00005f34	e58d3010	str	r3, [sp, #16]
00005f38	e1a00002	mov	r0, r2
00005f3c	eb0014e3	bl	0xb2d0	; symbol stub for: __ZN6google8protobuf15UnknownFieldSetD1Ev
00005f40	e59d3040	ldr	r3, [sp, #64]
00005f44	e58d3004	str	r3, [sp, #4]
00005f48	e3e03000	mvn	r3, #0	; 0x0
00005f4c	e58d3010	str	r3, [sp, #16]
00005f50	e59d0004	ldr	r0, [sp, #4]
00005f54	eb0014e3	bl	0xb2e8	; symbol stub for: __ZN6google8protobuf7MessageD2Ev
00005f58	e3a03001	mov	r3, #1	; 0x1
00005f5c	e6ef3073	uxtb r3,r3
00005f60	e3530000	cmp	r3, #0	; 0x0
00005f64	0a000012	beq	0x5fb4
00005f68	ea00000f	b	0x5fac
00005f6c	e59d3014	ldr	r3, [sp, #20]
00005f70	e58d3000	str	r3, [sp]
00005f74	e59d3000	ldr	r3, [sp]
00005f78	e58d3008	str	r3, [sp, #8]
00005f7c	e59d3040	ldr	r3, [sp, #64]
00005f80	e58d3004	str	r3, [sp, #4]
00005f84	e3a03000	mov	r3, #0	; 0x0
00005f88	e58d3010	str	r3, [sp, #16]
00005f8c	e59d0004	ldr	r0, [sp, #4]
00005f90	eb0014d4	bl	0xb2e8	; symbol stub for: __ZN6google8protobuf7MessageD2Ev
00005f94	e59d3008	ldr	r3, [sp, #8]
00005f98	e58d3000	str	r3, [sp]
00005f9c	e3e03000	mvn	r3, #0	; 0x0
00005fa0	e58d3010	str	r3, [sp, #16]
00005fa4	e59d0000	ldr	r0, [sp]
00005fa8	eb0014b3	bl	0xb27c	; symbol stub for: __Unwind_SjLj_Resume
00005fac	e59d0040	ldr	r0, [sp, #64]
00005fb0	eb0014e4	bl	0xb348	; symbol stub for: __ZdlPv
00005fb4	e28d300c	add	r3, sp, #12	; 0xc
00005fb8	e1a00003	mov	r0, r3
00005fbc	eb0014b1	bl	0xb288	; symbol stub for: __Unwind_SjLj_Unregister
00005fc0	e247d05c	sub	sp, r7, #92	; 0x5c
00005fc4	ecbd8b11	fldmiax	sp!, {d8-d15}
00005fc8	e247d018	sub	sp, r7, #24	; 0x18
00005fcc	e8bd0d00	ldmia	sp!, {r8, r10, r11}
00005fd0	e8bd80f0	ldmia	sp!, {r4, r5, r6, r7, pc}
00005fd4	00006140	andeq	r6, r0, r0, asr #2
00005fd8	000066c0	andeq	r6, r0, r0, asr #13
00005fdc	00000068	andeq	r0, r0, r8, rrx
00005fe0	000063e8	andeq	r6, r0, r8, ror #7
__ZN3dsp24AccelerometerDescription10descriptorEv:
00005fe4	e92d4080	stmdb	sp!, {r7, lr}
00005fe8	e28d7000	add	r7, sp, #0	; 0x0
00005fec	e59f3024	ldr	r3, [pc, #36]	; 0x6018
00005ff0	e08f3003	add	r3, pc, r3
00005ff4	e5933000	ldr	r3, [r3]
00005ff8	e3530000	cmp	r3, #0	; 0x0
00005ffc	1a000000	bne	0x6004
00006000	ebfff4fa	bl	__ZN3dsp30protobuf_BuildDesc_dsp_2eprotoEv
00006004	e59f3010	ldr	r3, [pc, #16]	; 0x601c
00006008	e08f3003	add	r3, pc, r3
0000600c	e5933000	ldr	r3, [r3]
00006010	e1a00003	mov	r0, r3
00006014	e8bd8080	ldmia	sp!, {r7, pc}
00006018	00006120	andeq	r6, r0, r0, lsr #2
0000601c	00006108	andeq	r6, r0, r8, lsl #2
__ZN3dsp24AccelerometerDescription16default_instanceEv:
00006020	e92d4080	stmdb	sp!, {r7, lr}
00006024	e28d7000	add	r7, sp, #0	; 0x0
00006028	e59f3024	ldr	r3, [pc, #36]	; 0x6054
0000602c	e08f3003	add	r3, pc, r3
00006030	e5933000	ldr	r3, [r3]
00006034	e3530000	cmp	r3, #0	; 0x0
00006038	1a000000	bne	0x6040
0000603c	ebfff4eb	bl	__ZN3dsp30protobuf_BuildDesc_dsp_2eprotoEv
00006040	e59f3010	ldr	r3, [pc, #16]	; 0x6058
00006044	e08f3003	add	r3, pc, r3
00006048	e5933000	ldr	r3, [r3]
0000604c	e1a00003	mov	r0, r3
00006050	e8bd8080	ldmia	sp!, {r7, pc}
00006054	000060ac	andeq	r6, r0, ip, lsr #1
00006058	00006094	muleq	r0, r4, r0
__ZNK3dsp24AccelerometerDescription3NewEv:
0000605c	e92d40f0	stmdb	sp!, {r4, r5, r6, r7, lr}
00006060	e28d700c	add	r7, sp, #12	; 0xc
00006064	e92d0d00	stmdb	sp!, {r8, r10, r11}
00006068	ed2d8b11	fstmdbx	sp!, {d8-d15}
0000606c	e24dd048	sub	sp, sp, #72	; 0x48
00006070	e58d0044	str	r0, [sp, #68]
00006074	e59f30c0	ldr	r3, [pc, #192]	; 0x613c
00006078	e08f3003	add	r3, pc, r3
0000607c	e5933000	ldr	r3, [r3]
00006080	e58d3028	str	r3, [sp, #40]
00006084	e59f30b4	ldr	r3, [pc, #180]	; 0x6140
00006088	e08f3003	add	r3, pc, r3
0000608c	e58d302c	str	r3, [sp, #44]
00006090	e28d2030	add	r2, sp, #48	; 0x30
00006094	e5827000	str	r7, [r2]
00006098	e59f30a4	ldr	r3, [pc, #164]	; 0x6144
0000609c	e08f3003	add	r3, pc, r3
000060a0	e5823004	str	r3, [r2, #4]
000060a4	e582d008	str	sp, [r2, #8]
000060a8	e28d3010	add	r3, sp, #16	; 0x10
000060ac	e1a00003	mov	r0, r3
000060b0	eb00146e	bl	0xb270	; symbol stub for: __Unwind_SjLj_Register
000060b4	e3e03000	mvn	r3, #0	; 0x0
000060b8	e58d3014	str	r3, [sp, #20]
000060bc	e3a00014	mov	r0, #20	; 0x14
000060c0	eb0014a3	bl	0xb354	; symbol stub for: __Znwm
000060c4	e1a03000	mov	r3, r0
000060c8	e58d3008	str	r3, [sp, #8]
000060cc	e3a03001	mov	r3, #1	; 0x1
000060d0	e58d3014	str	r3, [sp, #20]
000060d4	e59d0008	ldr	r0, [sp, #8]
000060d8	ebfff6c8	bl	__ZN3dsp24AccelerometerDescriptionC1Ev
000060dc	ea00000b	b	0x6110
000060e0	e59d3018	ldr	r3, [sp, #24]
000060e4	e58d3000	str	r3, [sp]
000060e8	e59d3000	ldr	r3, [sp]
000060ec	e58d300c	str	r3, [sp, #12]
000060f0	e59d0008	ldr	r0, [sp, #8]
000060f4	eb001493	bl	0xb348	; symbol stub for: __ZdlPv
000060f8	e59d300c	ldr	r3, [sp, #12]
000060fc	e58d3000	str	r3, [sp]
00006100	e3e03000	mvn	r3, #0	; 0x0
00006104	e58d3014	str	r3, [sp, #20]
00006108	e59d0000	ldr	r0, [sp]
0000610c	eb00145a	bl	0xb27c	; symbol stub for: __Unwind_SjLj_Resume
00006110	e59d3008	ldr	r3, [sp, #8]
00006114	e58d3004	str	r3, [sp, #4]
00006118	e28d3010	add	r3, sp, #16	; 0x10
0000611c	e1a00003	mov	r0, r3
00006120	eb001458	bl	0xb288	; symbol stub for: __Unwind_SjLj_Unregister
00006124	e59d0004	ldr	r0, [sp, #4]
00006128	e247d05c	sub	sp, r7, #92	; 0x5c
0000612c	ecbd8b11	fldmiax	sp!, {d8-d15}
00006130	e247d018	sub	sp, r7, #24	; 0x18
00006134	e8bd0d00	ldmia	sp!, {r8, r10, r11}
00006138	e8bd80f0	ldmia	sp!, {r4, r5, r6, r7, pc}
0000613c	00005fa0	andeq	r5, r0, r0, lsr #31
00006140	00006526	andeq	r6, r0, r6, lsr #10
00006144	0000003c	andeq	r0, r0, ip, lsr r0
__ZNK3dsp24AccelerometerDescription13GetDescriptorEv:
00006148	e92d4080	stmdb	sp!, {r7, lr}
0000614c	e28d7000	add	r7, sp, #0	; 0x0
00006150	e24dd004	sub	sp, sp, #4	; 0x4
00006154	e58d0000	str	r0, [sp]
00006158	ebffffa1	bl	__ZN3dsp24AccelerometerDescription10descriptorEv
0000615c	e1a03000	mov	r3, r0
00006160	e1a00003	mov	r0, r3
00006164	e247d000	sub	sp, r7, #0	; 0x0
00006168	e8bd8080	ldmia	sp!, {r7, pc}
__ZNK3dsp24AccelerometerDescription13GetReflectionEv:
0000616c	e92d4080	stmdb	sp!, {r7, lr}
00006170	e28d7000	add	r7, sp, #0	; 0x0
00006174	e24dd004	sub	sp, sp, #4	; 0x4
00006178	e58d0000	str	r0, [sp]
0000617c	e59f3028	ldr	r3, [pc, #40]	; 0x61ac
00006180	e08f3003	add	r3, pc, r3
00006184	e5933000	ldr	r3, [r3]
00006188	e3530000	cmp	r3, #0	; 0x0
0000618c	1a000000	bne	0x6194
00006190	ebfff496	bl	__ZN3dsp30protobuf_BuildDesc_dsp_2eprotoEv
00006194	e59f3014	ldr	r3, [pc, #20]	; 0x61b0
00006198	e08f3003	add	r3, pc, r3
0000619c	e5933000	ldr	r3, [r3]
000061a0	e1a00003	mov	r0, r3
000061a4	e247d000	sub	sp, r7, #0	; 0x0
000061a8	e8bd8080	ldmia	sp!, {r7, pc}
000061ac	00005f8c	andeq	r5, r0, ip, lsl #31
000061b0	00005f74	andeq	r5, r0, r4, ror pc
__ZN3dsp20AccelerometerMessageC2Ev:
000061b4	e92d40f0	stmdb	sp!, {r4, r5, r6, r7, lr}
000061b8	e28d700c	add	r7, sp, #12	; 0xc
000061bc	e92d0d00	stmdb	sp!, {r8, r10, r11}
000061c0	ed2d8b11	fstmdbx	sp!, {d8-d15}
000061c4	e24dd040	sub	sp, sp, #64	; 0x40
000061c8	e58d003c	str	r0, [sp, #60]
000061cc	e59f3130	ldr	r3, [pc, #304]	; 0x6304
000061d0	e08f3003	add	r3, pc, r3
000061d4	e5933000	ldr	r3, [r3]
000061d8	e58d3020	str	r3, [sp, #32]
000061dc	e59f3124	ldr	r3, [pc, #292]	; 0x6308
000061e0	e08f3003	add	r3, pc, r3
000061e4	e58d3024	str	r3, [sp, #36]
000061e8	e28d2028	add	r2, sp, #40	; 0x28
000061ec	e5827000	str	r7, [r2]
000061f0	e59f3114	ldr	r3, [pc, #276]	; 0x630c
000061f4	e08f3003	add	r3, pc, r3
000061f8	e5823004	str	r3, [r2, #4]
000061fc	e582d008	str	sp, [r2, #8]
00006200	e28d3008	add	r3, sp, #8	; 0x8
00006204	e1a00003	mov	r0, r3
00006208	eb001418	bl	0xb270	; symbol stub for: __Unwind_SjLj_Register
0000620c	e59d303c	ldr	r3, [sp, #60]
00006210	e1a00003	mov	r0, r3
00006214	e59f30f4	ldr	r3, [pc, #244]	; 0x6310
00006218	e08f3003	add	r3, pc, r3
0000621c	e5933000	ldr	r3, [r3]
00006220	e12fff33	blx	r3
00006224	e59f30e8	ldr	r3, [pc, #232]	; 0x6314
00006228	e08f3003	add	r3, pc, r3
0000622c	e2832008	add	r2, r3, #8	; 0x8
00006230	e59d303c	ldr	r3, [sp, #60]
00006234	e5832000	str	r2, [r3]
00006238	e59d303c	ldr	r3, [sp, #60]
0000623c	e2832004	add	r2, r3, #4	; 0x4
00006240	e3a03001	mov	r3, #1	; 0x1
00006244	e58d300c	str	r3, [sp, #12]
00006248	e1a00002	mov	r0, r2
0000624c	eb00141c	bl	0xb2c4	; symbol stub for: __ZN6google8protobuf15UnknownFieldSetC1Ev
00006250	e59d203c	ldr	r2, [sp, #60]
00006254	e3a03000	mov	r3, #0	; 0x0
00006258	e5823008	str	r3, [r2, #8]
0000625c	e59d103c	ldr	r1, [sp, #60]
00006260	e3a02000	mov	r2, #0	; 0x0
00006264	e3a03000	mov	r3, #0	; 0x0
00006268	e581200c	str	r2, [r1, #12]
0000626c	e5813010	str	r3, [r1, #16]
00006270	e59d203c	ldr	r2, [sp, #60]
00006274	e59f309c	ldr	r3, [pc, #156]	; 0x6318
00006278	e5823014	str	r3, [r2, #20]
0000627c	e59d203c	ldr	r2, [sp, #60]
00006280	e59f3090	ldr	r3, [pc, #144]	; 0x6318
00006284	e5823018	str	r3, [r2, #24]
00006288	e59d203c	ldr	r2, [sp, #60]
0000628c	e59f3084	ldr	r3, [pc, #132]	; 0x6318
00006290	e582301c	str	r3, [r2, #28]
00006294	e59d303c	ldr	r3, [sp, #60]
00006298	e2832020	add	r2, r3, #32	; 0x20
0000629c	e3a03000	mov	r3, #0	; 0x0
000062a0	e5823000	str	r3, [r2]
000062a4	ea00000e	b	0x62e4
000062a8	e59d3010	ldr	r3, [sp, #16]
000062ac	e58d3000	str	r3, [sp]
000062b0	e59d3000	ldr	r3, [sp]
000062b4	e58d3004	str	r3, [sp, #4]
000062b8	e59d203c	ldr	r2, [sp, #60]
000062bc	e3a03000	mov	r3, #0	; 0x0
000062c0	e58d300c	str	r3, [sp, #12]
000062c4	e1a00002	mov	r0, r2
000062c8	eb001406	bl	0xb2e8	; symbol stub for: __ZN6google8protobuf7MessageD2Ev
000062cc	e59d3004	ldr	r3, [sp, #4]
000062d0	e58d3000	str	r3, [sp]
000062d4	e3e03000	mvn	r3, #0	; 0x0
000062d8	e58d300c	str	r3, [sp, #12]
000062dc	e59d0000	ldr	r0, [sp]
000062e0	eb0013e5	bl	0xb27c	; symbol stub for: __Unwind_SjLj_Resume
000062e4	e28d3008	add	r3, sp, #8	; 0x8
000062e8	e1a00003	mov	r0, r3
000062ec	eb0013e5	bl	0xb288	; symbol stub for: __Unwind_SjLj_Unregister
000062f0	e247d05c	sub	sp, r7, #92	; 0x5c
000062f4	ecbd8b11	fldmiax	sp!, {d8-d15}
000062f8	e247d018	sub	sp, r7, #24	; 0x18
000062fc	e8bd0d00	ldmia	sp!, {r8, r10, r11}
00006300	e8bd80f0	ldmia	sp!, {r4, r5, r6, r7, pc}
00006304	00005e48	andeq	r5, r0, r8, asr #28
00006308	000063d4	ldreqd	r6, [r0], -r4
0000630c	000000ac	andeq	r0, r0, ip, lsr #1
00006310	00005e08	andeq	r5, r0, r8, lsl #28
00006314	00006120	andeq	r6, r0, r0, lsr #2
00006318	00000000	andeq	r0, r0, r0
__ZN3dsp20AccelerometerMessageC2ERKS0_:
0000631c	e92d40f0	stmdb	sp!, {r4, r5, r6, r7, lr}
00006320	e28d700c	add	r7, sp, #12	; 0xc
00006324	e92d0d00	stmdb	sp!, {r8, r10, r11}
00006328	ed2d8b11	fstmdbx	sp!, {d8-d15}
0000632c	e24dd048	sub	sp, sp, #72	; 0x48
00006330	e58d0044	str	r0, [sp, #68]
00006334	e58d1040	str	r1, [sp, #64]
00006338	e59f317c	ldr	r3, [pc, #380]	; 0x64bc
0000633c	e08f3003	add	r3, pc, r3
00006340	e5933000	ldr	r3, [r3]
00006344	e58d3024	str	r3, [sp, #36]
00006348	e59f3170	ldr	r3, [pc, #368]	; 0x64c0
0000634c	e08f3003	add	r3, pc, r3
00006350	e58d3028	str	r3, [sp, #40]
00006354	e28d202c	add	r2, sp, #44	; 0x2c
00006358	e5827000	str	r7, [r2]
0000635c	e59f3160	ldr	r3, [pc, #352]	; 0x64c4
00006360	e08f3003	add	r3, pc, r3
00006364	e5823004	str	r3, [r2, #4]
00006368	e582d008	str	sp, [r2, #8]
0000636c	e28d300c	add	r3, sp, #12	; 0xc
00006370	e1a00003	mov	r0, r3
00006374	eb0013bd	bl	0xb270	; symbol stub for: __Unwind_SjLj_Register
00006378	e59d3044	ldr	r3, [sp, #68]
0000637c	e1a00003	mov	r0, r3
00006380	e59f3140	ldr	r3, [pc, #320]	; 0x64c8
00006384	e08f3003	add	r3, pc, r3
00006388	e5933000	ldr	r3, [r3]
0000638c	e12fff33	blx	r3
00006390	e59f3134	ldr	r3, [pc, #308]	; 0x64cc
00006394	e08f3003	add	r3, pc, r3
00006398	e2832008	add	r2, r3, #8	; 0x8
0000639c	e59d3044	ldr	r3, [sp, #68]
000063a0	e5832000	str	r2, [r3]
000063a4	e59d3044	ldr	r3, [sp, #68]
000063a8	e2832004	add	r2, r3, #4	; 0x4
000063ac	e3a03002	mov	r3, #2	; 0x2
000063b0	e58d3010	str	r3, [sp, #16]
000063b4	e1a00002	mov	r0, r2
000063b8	eb0013c1	bl	0xb2c4	; symbol stub for: __ZN6google8protobuf15UnknownFieldSetC1Ev
000063bc	e59d2044	ldr	r2, [sp, #68]
000063c0	e3a03000	mov	r3, #0	; 0x0
000063c4	e5823008	str	r3, [r2, #8]
000063c8	e59d1044	ldr	r1, [sp, #68]
000063cc	e3a02000	mov	r2, #0	; 0x0
000063d0	e3a03000	mov	r3, #0	; 0x0
000063d4	e581200c	str	r2, [r1, #12]
000063d8	e5813010	str	r3, [r1, #16]
000063dc	e59d2044	ldr	r2, [sp, #68]
000063e0	e59f30e8	ldr	r3, [pc, #232]	; 0x64d0
000063e4	e5823014	str	r3, [r2, #20]
000063e8	e59d2044	ldr	r2, [sp, #68]
000063ec	e59f30dc	ldr	r3, [pc, #220]	; 0x64d0
000063f0	e5823018	str	r3, [r2, #24]
000063f4	e59d2044	ldr	r2, [sp, #68]
000063f8	e59f30d0	ldr	r3, [pc, #208]	; 0x64d0
000063fc	e582301c	str	r3, [r2, #28]
00006400	e59d3044	ldr	r3, [sp, #68]
00006404	e2832020	add	r2, r3, #32	; 0x20
00006408	e3a03000	mov	r3, #0	; 0x0
0000640c	e5823000	str	r3, [r2]
00006410	e59d2044	ldr	r2, [sp, #68]
00006414	e59d1040	ldr	r1, [sp, #64]
00006418	e3a03001	mov	r3, #1	; 0x1
0000641c	e58d3010	str	r3, [sp, #16]
00006420	e1a00002	mov	r0, r2
00006424	eb0013ac	bl	0xb2dc	; symbol stub for: __ZN6google8protobuf7Message9MergeFromERKS1_
00006428	ea00001b	b	0x649c
0000642c	e59d3010	ldr	r3, [sp, #16]
00006430	e59d2014	ldr	r2, [sp, #20]
00006434	e58d2000	str	r2, [sp]
00006438	e3530001	cmp	r3, #1	; 0x1
0000643c	0a000009	beq	0x6468
00006440	e59d3000	ldr	r3, [sp]
00006444	e58d3004	str	r3, [sp, #4]
00006448	e59d3044	ldr	r3, [sp, #68]
0000644c	e2832004	add	r2, r3, #4	; 0x4
00006450	e3a03000	mov	r3, #0	; 0x0
00006454	e58d3010	str	r3, [sp, #16]
00006458	e1a00002	mov	r0, r2
0000645c	eb00139b	bl	0xb2d0	; symbol stub for: __ZN6google8protobuf15UnknownFieldSetD1Ev
00006460	e59d2004	ldr	r2, [sp, #4]
00006464	e58d2000	str	r2, [sp]
00006468	e59d3000	ldr	r3, [sp]
0000646c	e58d3008	str	r3, [sp, #8]
00006470	e59d2044	ldr	r2, [sp, #68]
00006474	e3a03000	mov	r3, #0	; 0x0
00006478	e58d3010	str	r3, [sp, #16]
0000647c	e1a00002	mov	r0, r2
00006480	eb001398	bl	0xb2e8	; symbol stub for: __ZN6google8protobuf7MessageD2Ev
00006484	e59d2008	ldr	r2, [sp, #8]
00006488	e58d2000	str	r2, [sp]
0000648c	e3e03000	mvn	r3, #0	; 0x0
00006490	e58d3010	str	r3, [sp, #16]
00006494	e59d0000	ldr	r0, [sp]
00006498	eb001377	bl	0xb27c	; symbol stub for: __Unwind_SjLj_Resume
0000649c	e28d300c	add	r3, sp, #12	; 0xc
000064a0	e1a00003	mov	r0, r3
000064a4	eb001377	bl	0xb288	; symbol stub for: __Unwind_SjLj_Unregister
000064a8	e247d05c	sub	sp, r7, #92	; 0x5c
000064ac	ecbd8b11	fldmiax	sp!, {d8-d15}
000064b0	e247d018	sub	sp, r7, #24	; 0x18
000064b4	e8bd0d00	ldmia	sp!, {r8, r10, r11}
000064b8	e8bd80f0	ldmia	sp!, {r4, r5, r6, r7, pc}
000064bc	00005cdc	ldreqd	r5, [r0], -ip
000064c0	0000626e	andeq	r6, r0, lr, ror #4
000064c4	000000c4	andeq	r0, r0, r4, asr #1
000064c8	00005c9c	muleq	r0, ip, ip
000064cc	00005fb4	streqh	r5, [r0], -r4
000064d0	00000000	andeq	r0, r0, r0
__ZN3dsp20AccelerometerMessageC1ERKS0_:
000064d4	e92d40f0	stmdb	sp!, {r4, r5, r6, r7, lr}
000064d8	e28d700c	add	r7, sp, #12	; 0xc
000064dc	e92d0d00	stmdb	sp!, {r8, r10, r11}
000064e0	ed2d8b11	fstmdbx	sp!, {d8-d15}
000064e4	e24dd048	sub	sp, sp, #72	; 0x48
000064e8	e58d0044	str	r0, [sp, #68]
000064ec	e58d1040	str	r1, [sp, #64]
000064f0	e59f317c	ldr	r3, [pc, #380]	; 0x6674
000064f4	e08f3003	add	r3, pc, r3
000064f8	e5933000	ldr	r3, [r3]
000064fc	e58d3024	str	r3, [sp, #36]
00006500	e59f3170	ldr	r3, [pc, #368]	; 0x6678
00006504	e08f3003	add	r3, pc, r3
00006508	e58d3028	str	r3, [sp, #40]
0000650c	e28d202c	add	r2, sp, #44	; 0x2c
00006510	e5827000	str	r7, [r2]
00006514	e59f3160	ldr	r3, [pc, #352]	; 0x667c
00006518	e08f3003	add	r3, pc, r3
0000651c	e5823004	str	r3, [r2, #4]
00006520	e582d008	str	sp, [r2, #8]
00006524	e28d300c	add	r3, sp, #12	; 0xc
00006528	e1a00003	mov	r0, r3
0000652c	eb00134f	bl	0xb270	; symbol stub for: __Unwind_SjLj_Register
00006530	e59d3044	ldr	r3, [sp, #68]
00006534	e1a00003	mov	r0, r3
00006538	e59f3140	ldr	r3, [pc, #320]	; 0x6680
0000653c	e08f3003	add	r3, pc, r3
00006540	e5933000	ldr	r3, [r3]
00006544	e12fff33	blx	r3
00006548	e59f3134	ldr	r3, [pc, #308]	; 0x6684
0000654c	e08f3003	add	r3, pc, r3
00006550	e2832008	add	r2, r3, #8	; 0x8
00006554	e59d3044	ldr	r3, [sp, #68]
00006558	e5832000	str	r2, [r3]
0000655c	e59d3044	ldr	r3, [sp, #68]
00006560	e2832004	add	r2, r3, #4	; 0x4
00006564	e3a03002	mov	r3, #2	; 0x2
00006568	e58d3010	str	r3, [sp, #16]
0000656c	e1a00002	mov	r0, r2
00006570	eb001353	bl	0xb2c4	; symbol stub for: __ZN6google8protobuf15UnknownFieldSetC1Ev
00006574	e59d2044	ldr	r2, [sp, #68]
00006578	e3a03000	mov	r3, #0	; 0x0
0000657c	e5823008	str	r3, [r2, #8]
00006580	e59d1044	ldr	r1, [sp, #68]
00006584	e3a02000	mov	r2, #0	; 0x0
00006588	e3a03000	mov	r3, #0	; 0x0
0000658c	e581200c	str	r2, [r1, #12]
00006590	e5813010	str	r3, [r1, #16]
00006594	e59d2044	ldr	r2, [sp, #68]
00006598	e59f30e8	ldr	r3, [pc, #232]	; 0x6688
0000659c	e5823014	str	r3, [r2, #20]
000065a0	e59d2044	ldr	r2, [sp, #68]
000065a4	e59f30dc	ldr	r3, [pc, #220]	; 0x6688
000065a8	e5823018	str	r3, [r2, #24]
000065ac	e59d2044	ldr	r2, [sp, #68]
000065b0	e59f30d0	ldr	r3, [pc, #208]	; 0x6688
000065b4	e582301c	str	r3, [r2, #28]
000065b8	e59d3044	ldr	r3, [sp, #68]
000065bc	e2832020	add	r2, r3, #32	; 0x20
000065c0	e3a03000	mov	r3, #0	; 0x0
000065c4	e5823000	str	r3, [r2]
000065c8	e59d2044	ldr	r2, [sp, #68]
000065cc	e59d1040	ldr	r1, [sp, #64]
000065d0	e3a03001	mov	r3, #1	; 0x1
000065d4	e58d3010	str	r3, [sp, #16]
000065d8	e1a00002	mov	r0, r2
000065dc	eb00133e	bl	0xb2dc	; symbol stub for: __ZN6google8protobuf7Message9MergeFromERKS1_
000065e0	ea00001b	b	0x6654
000065e4	e59d3010	ldr	r3, [sp, #16]
000065e8	e59d2014	ldr	r2, [sp, #20]
000065ec	e58d2000	str	r2, [sp]
000065f0	e3530001	cmp	r3, #1	; 0x1
000065f4	0a000009	beq	0x6620
000065f8	e59d3000	ldr	r3, [sp]
000065fc	e58d3004	str	r3, [sp, #4]
00006600	e59d3044	ldr	r3, [sp, #68]
00006604	e2832004	add	r2, r3, #4	; 0x4
00006608	e3a03000	mov	r3, #0	; 0x0
0000660c	e58d3010	str	r3, [sp, #16]
00006610	e1a00002	mov	r0, r2
00006614	eb00132d	bl	0xb2d0	; symbol stub for: __ZN6google8protobuf15UnknownFieldSetD1Ev
00006618	e59d2004	ldr	r2, [sp, #4]
0000661c	e58d2000	str	r2, [sp]
00006620	e59d3000	ldr	r3, [sp]
00006624	e58d3008	str	r3, [sp, #8]
00006628	e59d2044	ldr	r2, [sp, #68]
0000662c	e3a03000	mov	r3, #0	; 0x0
00006630	e58d3010	str	r3, [sp, #16]
00006634	e1a00002	mov	r0, r2
00006638	eb00132a	bl	0xb2e8	; symbol stub for: __ZN6google8protobuf7MessageD2Ev
0000663c	e59d2008	ldr	r2, [sp, #8]
00006640	e58d2000	str	r2, [sp]
00006644	e3e03000	mvn	r3, #0	; 0x0
00006648	e58d3010	str	r3, [sp, #16]
0000664c	e59d0000	ldr	r0, [sp]
00006650	eb001309	bl	0xb27c	; symbol stub for: __Unwind_SjLj_Resume
00006654	e28d300c	add	r3, sp, #12	; 0xc
00006658	e1a00003	mov	r0, r3
0000665c	eb001309	bl	0xb288	; symbol stub for: __Unwind_SjLj_Unregister
00006660	e247d05c	sub	sp, r7, #92	; 0x5c
00006664	ecbd8b11	fldmiax	sp!, {d8-d15}
00006668	e247d018	sub	sp, r7, #24	; 0x18
0000666c	e8bd0d00	ldmia	sp!, {r8, r10, r11}
00006670	e8bd80f0	ldmia	sp!, {r4, r5, r6, r7, pc}
00006674	00005b24	andeq	r5, r0, r4, lsr #22
00006678	000060be	streqh	r6, [r0], -lr
0000667c	000000c4	andeq	r0, r0, r4, asr #1
00006680	00005ae4	andeq	r5, r0, r4, ror #21
00006684	00005dfc	streqd	r5, [r0], -ip
00006688	00000000	andeq	r0, r0, r0
__ZN3dsp20AccelerometerMessageD2Ev:
0000668c	e92d40f0	stmdb	sp!, {r4, r5, r6, r7, lr}
00006690	e28d700c	add	r7, sp, #12	; 0xc
00006694	e92d0d00	stmdb	sp!, {r8, r10, r11}
00006698	ed2d8b11	fstmdbx	sp!, {d8-d15}
0000669c	e24dd044	sub	sp, sp, #68	; 0x44
000066a0	e58d0040	str	r0, [sp, #64]
000066a4	e59f30f8	ldr	r3, [pc, #248]	; 0x67a4
000066a8	e08f3003	add	r3, pc, r3
000066ac	e5933000	ldr	r3, [r3]
000066b0	e58d3024	str	r3, [sp, #36]
000066b4	e59f30ec	ldr	r3, [pc, #236]	; 0x67a8
000066b8	e08f3003	add	r3, pc, r3
000066bc	e58d3028	str	r3, [sp, #40]
000066c0	e28d202c	add	r2, sp, #44	; 0x2c
000066c4	e5827000	str	r7, [r2]
000066c8	e59f30dc	ldr	r3, [pc, #220]	; 0x67ac
000066cc	e08f3003	add	r3, pc, r3
000066d0	e5823004	str	r3, [r2, #4]
000066d4	e582d008	str	sp, [r2, #8]
000066d8	e28d300c	add	r3, sp, #12	; 0xc
000066dc	e1a00003	mov	r0, r3
000066e0	eb0012e2	bl	0xb270	; symbol stub for: __Unwind_SjLj_Register
000066e4	e59f30c4	ldr	r3, [pc, #196]	; 0x67b0
000066e8	e08f3003	add	r3, pc, r3
000066ec	e2832008	add	r2, r3, #8	; 0x8
000066f0	e59d3040	ldr	r3, [sp, #64]
000066f4	e5832000	str	r2, [r3]
000066f8	e59d3040	ldr	r3, [sp, #64]
000066fc	e2832004	add	r2, r3, #4	; 0x4
00006700	e3a03001	mov	r3, #1	; 0x1
00006704	e58d3010	str	r3, [sp, #16]
00006708	e1a00002	mov	r0, r2
0000670c	eb0012ef	bl	0xb2d0	; symbol stub for: __ZN6google8protobuf15UnknownFieldSetD1Ev
00006710	e59d3040	ldr	r3, [sp, #64]
00006714	e58d3004	str	r3, [sp, #4]
00006718	e3e03000	mvn	r3, #0	; 0x0
0000671c	e58d3010	str	r3, [sp, #16]
00006720	e59d0004	ldr	r0, [sp, #4]
00006724	eb0012ef	bl	0xb2e8	; symbol stub for: __ZN6google8protobuf7MessageD2Ev
00006728	e3a03000	mov	r3, #0	; 0x0
0000672c	e6ef3073	uxtb r3,r3
00006730	e3530000	cmp	r3, #0	; 0x0
00006734	0a000012	beq	0x6784
00006738	ea00000f	b	0x677c
0000673c	e59d3014	ldr	r3, [sp, #20]
00006740	e58d3000	str	r3, [sp]
00006744	e59d3000	ldr	r3, [sp]
00006748	e58d3008	str	r3, [sp, #8]
0000674c	e59d3040	ldr	r3, [sp, #64]
00006750	e58d3004	str	r3, [sp, #4]
00006754	e3a03000	mov	r3, #0	; 0x0
00006758	e58d3010	str	r3, [sp, #16]
0000675c	e59d0004	ldr	r0, [sp, #4]
00006760	eb0012e0	bl	0xb2e8	; symbol stub for: __ZN6google8protobuf7MessageD2Ev
00006764	e59d3008	ldr	r3, [sp, #8]
00006768	e58d3000	str	r3, [sp]
0000676c	e3e03000	mvn	r3, #0	; 0x0
00006770	e58d3010	str	r3, [sp, #16]
00006774	e59d0000	ldr	r0, [sp]
00006778	eb0012bf	bl	0xb27c	; symbol stub for: __Unwind_SjLj_Resume
0000677c	e59d0040	ldr	r0, [sp, #64]
00006780	eb0012f0	bl	0xb348	; symbol stub for: __ZdlPv
00006784	e28d300c	add	r3, sp, #12	; 0xc
00006788	e1a00003	mov	r0, r3
0000678c	eb0012bd	bl	0xb288	; symbol stub for: __Unwind_SjLj_Unregister
00006790	e247d05c	sub	sp, r7, #92	; 0x5c
00006794	ecbd8b11	fldmiax	sp!, {d8-d15}
00006798	e247d018	sub	sp, r7, #24	; 0x18
0000679c	e8bd0d00	ldmia	sp!, {r8, r10, r11}
000067a0	e8bd80f0	ldmia	sp!, {r4, r5, r6, r7, pc}
000067a4	00005970	andeq	r5, r0, r0, ror r9
000067a8	00005f12	andeq	r5, r0, r2, lsl pc
000067ac	00000068	andeq	r0, r0, r8, rrx
000067b0	00005c60	andeq	r5, r0, r0, ror #24
__ZN3dsp20AccelerometerMessageD1Ev:
000067b4	e92d40f0	stmdb	sp!, {r4, r5, r6, r7, lr}
000067b8	e28d700c	add	r7, sp, #12	; 0xc
000067bc	e92d0d00	stmdb	sp!, {r8, r10, r11}
000067c0	ed2d8b11	fstmdbx	sp!, {d8-d15}
000067c4	e24dd044	sub	sp, sp, #68	; 0x44
000067c8	e58d0040	str	r0, [sp, #64]
000067cc	e59f30f8	ldr	r3, [pc, #248]	; 0x68cc
000067d0	e08f3003	add	r3, pc, r3
000067d4	e5933000	ldr	r3, [r3]
000067d8	e58d3024	str	r3, [sp, #36]
000067dc	e59f30ec	ldr	r3, [pc, #236]	; 0x68d0
000067e0	e08f3003	add	r3, pc, r3
000067e4	e58d3028	str	r3, [sp, #40]
000067e8	e28d202c	add	r2, sp, #44	; 0x2c
000067ec	e5827000	str	r7, [r2]
000067f0	e59f30dc	ldr	r3, [pc, #220]	; 0x68d4
000067f4	e08f3003	add	r3, pc, r3
000067f8	e5823004	str	r3, [r2, #4]
000067fc	e582d008	str	sp, [r2, #8]
00006800	e28d300c	add	r3, sp, #12	; 0xc
00006804	e1a00003	mov	r0, r3
00006808	eb001298	bl	0xb270	; symbol stub for: __Unwind_SjLj_Register
0000680c	e59f30c4	ldr	r3, [pc, #196]	; 0x68d8
00006810	e08f3003	add	r3, pc, r3
00006814	e2832008	add	r2, r3, #8	; 0x8
00006818	e59d3040	ldr	r3, [sp, #64]
0000681c	e5832000	str	r2, [r3]
00006820	e59d3040	ldr	r3, [sp, #64]
00006824	e2832004	add	r2, r3, #4	; 0x4
00006828	e3a03001	mov	r3, #1	; 0x1
0000682c	e58d3010	str	r3, [sp, #16]
00006830	e1a00002	mov	r0, r2
00006834	eb0012a5	bl	0xb2d0	; symbol stub for: __ZN6google8protobuf15UnknownFieldSetD1Ev
00006838	e59d3040	ldr	r3, [sp, #64]
0000683c	e58d3004	str	r3, [sp, #4]
00006840	e3e03000	mvn	r3, #0	; 0x0
00006844	e58d3010	str	r3, [sp, #16]
00006848	e59d0004	ldr	r0, [sp, #4]
0000684c	eb0012a5	bl	0xb2e8	; symbol stub for: __ZN6google8protobuf7MessageD2Ev
00006850	e3a03000	mov	r3, #0	; 0x0
00006854	e6ef3073	uxtb r3,r3
00006858	e3530000	cmp	r3, #0	; 0x0
0000685c	0a000012	beq	0x68ac
00006860	ea00000f	b	0x68a4
00006864	e59d3014	ldr	r3, [sp, #20]
00006868	e58d3000	str	r3, [sp]
0000686c	e59d3000	ldr	r3, [sp]
00006870	e58d3008	str	r3, [sp, #8]
00006874	e59d3040	ldr	r3, [sp, #64]
00006878	e58d3004	str	r3, [sp, #4]
0000687c	e3a03000	mov	r3, #0	; 0x0
00006880	e58d3010	str	r3, [sp, #16]
00006884	e59d0004	ldr	r0, [sp, #4]
00006888	eb001296	bl	0xb2e8	; symbol stub for: __ZN6google8protobuf7MessageD2Ev
0000688c	e59d3008	ldr	r3, [sp, #8]
00006890	e58d3000	str	r3, [sp]
00006894	e3e03000	mvn	r3, #0	; 0x0
00006898	e58d3010	str	r3, [sp, #16]
0000689c	e59d0000	ldr	r0, [sp]
000068a0	eb001275	bl	0xb27c	; symbol stub for: __Unwind_SjLj_Resume
000068a4	e59d0040	ldr	r0, [sp, #64]
000068a8	eb0012a6	bl	0xb348	; symbol stub for: __ZdlPv
000068ac	e28d300c	add	r3, sp, #12	; 0xc
000068b0	e1a00003	mov	r0, r3
000068b4	eb001273	bl	0xb288	; symbol stub for: __Unwind_SjLj_Unregister
000068b8	e247d05c	sub	sp, r7, #92	; 0x5c
000068bc	ecbd8b11	fldmiax	sp!, {d8-d15}
000068c0	e247d018	sub	sp, r7, #24	; 0x18
000068c4	e8bd0d00	ldmia	sp!, {r8, r10, r11}
000068c8	e8bd80f0	ldmia	sp!, {r4, r5, r6, r7, pc}
000068cc	00005848	andeq	r5, r0, r8, asr #16
000068d0	00005df0	streqd	r5, [r0], -r0
000068d4	00000068	andeq	r0, r0, r8, rrx
000068d8	00005b38	andeq	r5, r0, r8, lsr r11
__ZN3dsp20AccelerometerMessageD0Ev:
000068dc	e92d40f0	stmdb	sp!, {r4, r5, r6, r7, lr}
000068e0	e28d700c	add	r7, sp, #12	; 0xc
000068e4	e92d0d00	stmdb	sp!, {r8, r10, r11}
000068e8	ed2d8b11	fstmdbx	sp!, {d8-d15}
000068ec	e24dd044	sub	sp, sp, #68	; 0x44
000068f0	e58d0040	str	r0, [sp, #64]
000068f4	e59f30f8	ldr	r3, [pc, #248]	; 0x69f4
000068f8	e08f3003	add	r3, pc, r3
000068fc	e5933000	ldr	r3, [r3]
00006900	e58d3024	str	r3, [sp, #36]
00006904	e59f30ec	ldr	r3, [pc, #236]	; 0x69f8
00006908	e08f3003	add	r3, pc, r3
0000690c	e58d3028	str	r3, [sp, #40]
00006910	e28d202c	add	r2, sp, #44	; 0x2c
00006914	e5827000	str	r7, [r2]
00006918	e59f30dc	ldr	r3, [pc, #220]	; 0x69fc
0000691c	e08f3003	add	r3, pc, r3
00006920	e5823004	str	r3, [r2, #4]
00006924	e582d008	str	sp, [r2, #8]
00006928	e28d300c	add	r3, sp, #12	; 0xc
0000692c	e1a00003	mov	r0, r3
00006930	eb00124e	bl	0xb270	; symbol stub for: __Unwind_SjLj_Register
00006934	e59f30c4	ldr	r3, [pc, #196]	; 0x6a00
00006938	e08f3003	add	r3, pc, r3
0000693c	e2832008	add	r2, r3, #8	; 0x8
00006940	e59d3040	ldr	r3, [sp, #64]
00006944	e5832000	str	r2, [r3]
00006948	e59d3040	ldr	r3, [sp, #64]
0000694c	e2832004	add	r2, r3, #4	; 0x4
00006950	e3a03001	mov	r3, #1	; 0x1
00006954	e58d3010	str	r3, [sp, #16]
00006958	e1a00002	mov	r0, r2
0000695c	eb00125b	bl	0xb2d0	; symbol stub for: __ZN6google8protobuf15UnknownFieldSetD1Ev
00006960	e59d3040	ldr	r3, [sp, #64]
00006964	e58d3004	str	r3, [sp, #4]
00006968	e3e03000	mvn	r3, #0	; 0x0
0000696c	e58d3010	str	r3, [sp, #16]
00006970	e59d0004	ldr	r0, [sp, #4]
00006974	eb00125b	bl	0xb2e8	; symbol stub for: __ZN6google8protobuf7MessageD2Ev
00006978	e3a03001	mov	r3, #1	; 0x1
0000697c	e6ef3073	uxtb r3,r3
00006980	e3530000	cmp	r3, #0	; 0x0
00006984	0a000012	beq	0x69d4
00006988	ea00000f	b	0x69cc
0000698c	e59d3014	ldr	r3, [sp, #20]
00006990	e58d3000	str	r3, [sp]
00006994	e59d3000	ldr	r3, [sp]
00006998	e58d3008	str	r3, [sp, #8]
0000699c	e59d3040	ldr	r3, [sp, #64]
000069a0	e58d3004	str	r3, [sp, #4]
000069a4	e3a03000	mov	r3, #0	; 0x0
000069a8	e58d3010	str	r3, [sp, #16]
000069ac	e59d0004	ldr	r0, [sp, #4]
000069b0	eb00124c	bl	0xb2e8	; symbol stub for: __ZN6google8protobuf7MessageD2Ev
000069b4	e59d3008	ldr	r3, [sp, #8]
000069b8	e58d3000	str	r3, [sp]
000069bc	e3e03000	mvn	r3, #0	; 0x0
000069c0	e58d3010	str	r3, [sp, #16]
000069c4	e59d0000	ldr	r0, [sp]
000069c8	eb00122b	bl	0xb27c	; symbol stub for: __Unwind_SjLj_Resume
000069cc	e59d0040	ldr	r0, [sp, #64]
000069d0	eb00125c	bl	0xb348	; symbol stub for: __ZdlPv
000069d4	e28d300c	add	r3, sp, #12	; 0xc
000069d8	e1a00003	mov	r0, r3
000069dc	eb001229	bl	0xb288	; symbol stub for: __Unwind_SjLj_Unregister
000069e0	e247d05c	sub	sp, r7, #92	; 0x5c
000069e4	ecbd8b11	fldmiax	sp!, {d8-d15}
000069e8	e247d018	sub	sp, r7, #24	; 0x18
000069ec	e8bd0d00	ldmia	sp!, {r8, r10, r11}
000069f0	e8bd80f0	ldmia	sp!, {r4, r5, r6, r7, pc}
000069f4	00005720	andeq	r5, r0, r0, lsr #14
000069f8	00005cce	andeq	r5, r0, lr, asr #25
000069fc	00000068	andeq	r0, r0, r8, rrx
00006a00	00005a10	andeq	r5, r0, r0, lsl r10
__ZN3dsp20AccelerometerMessage10descriptorEv:
00006a04	e92d4080	stmdb	sp!, {r7, lr}
00006a08	e28d7000	add	r7, sp, #0	; 0x0
00006a0c	e59f3024	ldr	r3, [pc, #36]	; 0x6a38
00006a10	e08f3003	add	r3, pc, r3
00006a14	e5933000	ldr	r3, [r3]
00006a18	e3530000	cmp	r3, #0	; 0x0
00006a1c	1a000000	bne	0x6a24
00006a20	ebfff272	bl	__ZN3dsp30protobuf_BuildDesc_dsp_2eprotoEv
00006a24	e59f3010	ldr	r3, [pc, #16]	; 0x6a3c
00006a28	e08f3003	add	r3, pc, r3
00006a2c	e5933000	ldr	r3, [r3]
00006a30	e1a00003	mov	r0, r3
00006a34	e8bd8080	ldmia	sp!, {r7, pc}
00006a38	000056f8	streqd	r5, [r0], -r8
00006a3c	000056e0	andeq	r5, r0, r0, ror #13
__ZN3dsp20AccelerometerMessage16default_instanceEv:
00006a40	e92d4080	stmdb	sp!, {r7, lr}
00006a44	e28d7000	add	r7, sp, #0	; 0x0
00006a48	e59f3024	ldr	r3, [pc, #36]	; 0x6a74
00006a4c	e08f3003	add	r3, pc, r3
00006a50	e5933000	ldr	r3, [r3]
00006a54	e3530000	cmp	r3, #0	; 0x0
00006a58	1a000000	bne	0x6a60
00006a5c	ebfff263	bl	__ZN3dsp30protobuf_BuildDesc_dsp_2eprotoEv
00006a60	e59f3010	ldr	r3, [pc, #16]	; 0x6a78
00006a64	e08f3003	add	r3, pc, r3
00006a68	e5933000	ldr	r3, [r3]
00006a6c	e1a00003	mov	r0, r3
00006a70	e8bd8080	ldmia	sp!, {r7, pc}
00006a74	00005688	andeq	r5, r0, r8, lsl #13
00006a78	00005670	andeq	r5, r0, r0, ror r6
__ZNK3dsp20AccelerometerMessage3NewEv:
00006a7c	e92d40f0	stmdb	sp!, {r4, r5, r6, r7, lr}
00006a80	e28d700c	add	r7, sp, #12	; 0xc
00006a84	e92d0d00	stmdb	sp!, {r8, r10, r11}
00006a88	ed2d8b11	fstmdbx	sp!, {d8-d15}
00006a8c	e24dd048	sub	sp, sp, #72	; 0x48
00006a90	e58d0044	str	r0, [sp, #68]
00006a94	e59f30c0	ldr	r3, [pc, #192]	; 0x6b5c
00006a98	e08f3003	add	r3, pc, r3
00006a9c	e5933000	ldr	r3, [r3]
00006aa0	e58d3028	str	r3, [sp, #40]
00006aa4	e59f30b4	ldr	r3, [pc, #180]	; 0x6b60
00006aa8	e08f3003	add	r3, pc, r3
00006aac	e58d302c	str	r3, [sp, #44]
00006ab0	e28d2030	add	r2, sp, #48	; 0x30
00006ab4	e5827000	str	r7, [r2]
00006ab8	e59f30a4	ldr	r3, [pc, #164]	; 0x6b64
00006abc	e08f3003	add	r3, pc, r3
00006ac0	e5823004	str	r3, [r2, #4]
00006ac4	e582d008	str	sp, [r2, #8]
00006ac8	e28d3010	add	r3, sp, #16	; 0x10
00006acc	e1a00003	mov	r0, r3
00006ad0	eb0011e6	bl	0xb270	; symbol stub for: __Unwind_SjLj_Register
00006ad4	e3e03000	mvn	r3, #0	; 0x0
00006ad8	e58d3014	str	r3, [sp, #20]
00006adc	e3a00024	mov	r0, #36	; 0x24
00006ae0	eb00121b	bl	0xb354	; symbol stub for: __Znwm
00006ae4	e1a03000	mov	r3, r0
00006ae8	e58d3008	str	r3, [sp, #8]
00006aec	e3a03001	mov	r3, #1	; 0x1
00006af0	e58d3014	str	r3, [sp, #20]
00006af4	e59d0008	ldr	r0, [sp, #8]
00006af8	ebfff3e6	bl	__ZN3dsp20AccelerometerMessageC1Ev
00006afc	ea00000b	b	0x6b30
00006b00	e59d3018	ldr	r3, [sp, #24]
00006b04	e58d3000	str	r3, [sp]
00006b08	e59d3000	ldr	r3, [sp]
00006b0c	e58d300c	str	r3, [sp, #12]
00006b10	e59d0008	ldr	r0, [sp, #8]
00006b14	eb00120b	bl	0xb348	; symbol stub for: __ZdlPv
00006b18	e59d300c	ldr	r3, [sp, #12]
00006b1c	e58d3000	str	r3, [sp]
00006b20	e3e03000	mvn	r3, #0	; 0x0
00006b24	e58d3014	str	r3, [sp, #20]
00006b28	e59d0000	ldr	r0, [sp]
00006b2c	eb0011d2	bl	0xb27c	; symbol stub for: __Unwind_SjLj_Resume
00006b30	e59d3008	ldr	r3, [sp, #8]
00006b34	e58d3004	str	r3, [sp, #4]
00006b38	e28d3010	add	r3, sp, #16	; 0x10
00006b3c	e1a00003	mov	r0, r3
00006b40	eb0011d0	bl	0xb288	; symbol stub for: __Unwind_SjLj_Unregister
00006b44	e59d0004	ldr	r0, [sp, #4]
00006b48	e247d05c	sub	sp, r7, #92	; 0x5c
00006b4c	ecbd8b11	fldmiax	sp!, {d8-d15}
00006b50	e247d018	sub	sp, r7, #24	; 0x18
00006b54	e8bd0d00	ldmia	sp!, {r8, r10, r11}
00006b58	e8bd80f0	ldmia	sp!, {r4, r5, r6, r7, pc}
00006b5c	00005580	andeq	r5, r0, r0, lsl #11
00006b60	00005b34	andeq	r5, r0, r4, lsr r11
00006b64	0000003c	andeq	r0, r0, ip, lsr r0
__ZNK3dsp20AccelerometerMessage13GetDescriptorEv:
00006b68	e92d4080	stmdb	sp!, {r7, lr}
00006b6c	e28d7000	add	r7, sp, #0	; 0x0
00006b70	e24dd004	sub	sp, sp, #4	; 0x4
00006b74	e58d0000	str	r0, [sp]
00006b78	ebffffa1	bl	__ZN3dsp20AccelerometerMessage10descriptorEv
00006b7c	e1a03000	mov	r3, r0
00006b80	e1a00003	mov	r0, r3
00006b84	e247d000	sub	sp, r7, #0	; 0x0
00006b88	e8bd8080	ldmia	sp!, {r7, pc}
__ZNK3dsp20AccelerometerMessage13GetReflectionEv:
00006b8c	e92d4080	stmdb	sp!, {r7, lr}
00006b90	e28d7000	add	r7, sp, #0	; 0x0
00006b94	e24dd004	sub	sp, sp, #4	; 0x4
00006b98	e58d0000	str	r0, [sp]
00006b9c	e59f3028	ldr	r3, [pc, #40]	; 0x6bcc
00006ba0	e08f3003	add	r3, pc, r3
00006ba4	e5933000	ldr	r3, [r3]
00006ba8	e3530000	cmp	r3, #0	; 0x0
00006bac	1a000000	bne	0x6bb4
00006bb0	ebfff20e	bl	__ZN3dsp30protobuf_BuildDesc_dsp_2eprotoEv
00006bb4	e59f3014	ldr	r3, [pc, #20]	; 0x6bd0
00006bb8	e08f3003	add	r3, pc, r3
00006bbc	e5933000	ldr	r3, [r3]
00006bc0	e1a00003	mov	r0, r3
00006bc4	e247d000	sub	sp, r7, #0	; 0x0
00006bc8	e8bd8080	ldmia	sp!, {r7, pc}
00006bcc	00005564	andeq	r5, r0, r4, ror #10
00006bd0	0000554c	andeq	r5, r0, ip, asr #10
__ZN3dsp21MicrophoneDescriptionC2Ev:
00006bd4	e92d40f0	stmdb	sp!, {r4, r5, r6, r7, lr}
00006bd8	e28d700c	add	r7, sp, #12	; 0xc
00006bdc	e92d0d00	stmdb	sp!, {r8, r10, r11}
00006be0	ed2d8b11	fstmdbx	sp!, {d8-d15}
00006be4	e24dd040	sub	sp, sp, #64	; 0x40
00006be8	e58d003c	str	r0, [sp, #60]
00006bec	e59f311c	ldr	r3, [pc, #284]	; 0x6d10
00006bf0	e08f3003	add	r3, pc, r3
00006bf4	e5933000	ldr	r3, [r3]
00006bf8	e58d3020	str	r3, [sp, #32]
00006bfc	e59f3110	ldr	r3, [pc, #272]	; 0x6d14
00006c00	e08f3003	add	r3, pc, r3
00006c04	e58d3024	str	r3, [sp, #36]
00006c08	e28d2028	add	r2, sp, #40	; 0x28
00006c0c	e5827000	str	r7, [r2]
00006c10	e59f3100	ldr	r3, [pc, #256]	; 0x6d18
00006c14	e08f3003	add	r3, pc, r3
00006c18	e5823004	str	r3, [r2, #4]
00006c1c	e582d008	str	sp, [r2, #8]
00006c20	e28d3008	add	r3, sp, #8	; 0x8
00006c24	e1a00003	mov	r0, r3
00006c28	eb001190	bl	0xb270	; symbol stub for: __Unwind_SjLj_Register
00006c2c	e59d303c	ldr	r3, [sp, #60]
00006c30	e1a00003	mov	r0, r3
00006c34	e59f30e0	ldr	r3, [pc, #224]	; 0x6d1c
00006c38	e08f3003	add	r3, pc, r3
00006c3c	e5933000	ldr	r3, [r3]
00006c40	e12fff33	blx	r3
00006c44	e59f30d4	ldr	r3, [pc, #212]	; 0x6d20
00006c48	e08f3003	add	r3, pc, r3
00006c4c	e2832008	add	r2, r3, #8	; 0x8
00006c50	e59d303c	ldr	r3, [sp, #60]
00006c54	e5832000	str	r2, [r3]
00006c58	e59d303c	ldr	r3, [sp, #60]
00006c5c	e2832004	add	r2, r3, #4	; 0x4
00006c60	e3a03001	mov	r3, #1	; 0x1
00006c64	e58d300c	str	r3, [sp, #12]
00006c68	e1a00002	mov	r0, r2
00006c6c	eb001194	bl	0xb2c4	; symbol stub for: __ZN6google8protobuf15UnknownFieldSetC1Ev
00006c70	e59d203c	ldr	r2, [sp, #60]
00006c74	e3a03000	mov	r3, #0	; 0x0
00006c78	e5823008	str	r3, [r2, #8]
00006c7c	e59d203c	ldr	r2, [sp, #60]
00006c80	e59f309c	ldr	r3, [pc, #156]	; 0x6d24
00006c84	e582300c	str	r3, [r2, #12]
00006c88	e59d203c	ldr	r2, [sp, #60]
00006c8c	e3a03000	mov	r3, #0	; 0x0
00006c90	e5823010	str	r3, [r2, #16]
00006c94	e59d203c	ldr	r2, [sp, #60]
00006c98	e3a03000	mov	r3, #0	; 0x0
00006c9c	e5823014	str	r3, [r2, #20]
00006ca0	e59d303c	ldr	r3, [sp, #60]
00006ca4	e2832018	add	r2, r3, #24	; 0x18
00006ca8	e3a03000	mov	r3, #0	; 0x0
00006cac	e5823000	str	r3, [r2]
00006cb0	ea00000e	b	0x6cf0
00006cb4	e59d3010	ldr	r3, [sp, #16]
00006cb8	e58d3000	str	r3, [sp]
00006cbc	e59d3000	ldr	r3, [sp]
00006cc0	e58d3004	str	r3, [sp, #4]
00006cc4	e59d203c	ldr	r2, [sp, #60]
00006cc8	e3a03000	mov	r3, #0	; 0x0
00006ccc	e58d300c	str	r3, [sp, #12]
00006cd0	e1a00002	mov	r0, r2
00006cd4	eb001183	bl	0xb2e8	; symbol stub for: __ZN6google8protobuf7MessageD2Ev
00006cd8	e59d3004	ldr	r3, [sp, #4]
00006cdc	e58d3000	str	r3, [sp]
00006ce0	e3e03000	mvn	r3, #0	; 0x0
00006ce4	e58d300c	str	r3, [sp, #12]
00006ce8	e59d0000	ldr	r0, [sp]
00006cec	eb001162	bl	0xb27c	; symbol stub for: __Unwind_SjLj_Resume
00006cf0	e28d3008	add	r3, sp, #8	; 0x8
00006cf4	e1a00003	mov	r0, r3
00006cf8	eb001162	bl	0xb288	; symbol stub for: __Unwind_SjLj_Unregister
00006cfc	e247d05c	sub	sp, r7, #92	; 0x5c
00006d00	ecbd8b11	fldmiax	sp!, {d8-d15}
00006d04	e247d018	sub	sp, r7, #24	; 0x18
00006d08	e8bd0d00	ldmia	sp!, {r8, r10, r11}
00006d0c	e8bd80f0	ldmia	sp!, {r4, r5, r6, r7, pc}
00006d10	00005428	andeq	r5, r0, r8, lsr #8
00006d14	000059e2	andeq	r5, r0, r2, ror #19
00006d18	00000098	muleq	r0, r8, r0
00006d1c	000053e8	andeq	r5, r0, r8, ror #7
00006d20	00005748	andeq	r5, r0, r8, asr #14
00006d24	00000000	andeq	r0, r0, r0
__ZN3dsp21MicrophoneDescriptionC2ERKS0_:
00006d28	e92d40f0	stmdb	sp!, {r4, r5, r6, r7, lr}
00006d2c	e28d700c	add	r7, sp, #12	; 0xc
00006d30	e92d0d00	stmdb	sp!, {r8, r10, r11}
00006d34	ed2d8b11	fstmdbx	sp!, {d8-d15}
00006d38	e24dd048	sub	sp, sp, #72	; 0x48
00006d3c	e58d0044	str	r0, [sp, #68]
00006d40	e58d1040	str	r1, [sp, #64]
00006d44	e59f3168	ldr	r3, [pc, #360]	; 0x6eb4
00006d48	e08f3003	add	r3, pc, r3
00006d4c	e5933000	ldr	r3, [r3]
00006d50	e58d3024	str	r3, [sp, #36]
00006d54	e59f315c	ldr	r3, [pc, #348]	; 0x6eb8
00006d58	e08f3003	add	r3, pc, r3
00006d5c	e58d3028	str	r3, [sp, #40]
00006d60	e28d202c	add	r2, sp, #44	; 0x2c
00006d64	e5827000	str	r7, [r2]
00006d68	e59f314c	ldr	r3, [pc, #332]	; 0x6ebc
00006d6c	e08f3003	add	r3, pc, r3
00006d70	e5823004	str	r3, [r2, #4]
00006d74	e582d008	str	sp, [r2, #8]
00006d78	e28d300c	add	r3, sp, #12	; 0xc
00006d7c	e1a00003	mov	r0, r3
00006d80	eb00113a	bl	0xb270	; symbol stub for: __Unwind_SjLj_Register
00006d84	e59d3044	ldr	r3, [sp, #68]
00006d88	e1a00003	mov	r0, r3
00006d8c	e59f312c	ldr	r3, [pc, #300]	; 0x6ec0
00006d90	e08f3003	add	r3, pc, r3
00006d94	e5933000	ldr	r3, [r3]
00006d98	e12fff33	blx	r3
00006d9c	e59f3120	ldr	r3, [pc, #288]	; 0x6ec4
00006da0	e08f3003	add	r3, pc, r3
00006da4	e2832008	add	r2, r3, #8	; 0x8
00006da8	e59d3044	ldr	r3, [sp, #68]
00006dac	e5832000	str	r2, [r3]
00006db0	e59d3044	ldr	r3, [sp, #68]
00006db4	e2832004	add	r2, r3, #4	; 0x4
00006db8	e3a03002	mov	r3, #2	; 0x2
00006dbc	e58d3010	str	r3, [sp, #16]
00006dc0	e1a00002	mov	r0, r2
00006dc4	eb00113e	bl	0xb2c4	; symbol stub for: __ZN6google8protobuf15UnknownFieldSetC1Ev
00006dc8	e59d2044	ldr	r2, [sp, #68]
00006dcc	e3a03000	mov	r3, #0	; 0x0
00006dd0	e5823008	str	r3, [r2, #8]
00006dd4	e59d2044	ldr	r2, [sp, #68]
00006dd8	e59f30e8	ldr	r3, [pc, #232]	; 0x6ec8
00006ddc	e582300c	str	r3, [r2, #12]
00006de0	e59d2044	ldr	r2, [sp, #68]
00006de4	e3a03000	mov	r3, #0	; 0x0
00006de8	e5823010	str	r3, [r2, #16]
00006dec	e59d2044	ldr	r2, [sp, #68]
00006df0	e3a03000	mov	r3, #0	; 0x0
00006df4	e5823014	str	r3, [r2, #20]
00006df8	e59d3044	ldr	r3, [sp, #68]
00006dfc	e2832018	add	r2, r3, #24	; 0x18
00006e00	e3a03000	mov	r3, #0	; 0x0
00006e04	e5823000	str	r3, [r2]
00006e08	e59d2044	ldr	r2, [sp, #68]
00006e0c	e59d1040	ldr	r1, [sp, #64]
00006e10	e3a03001	mov	r3, #1	; 0x1
00006e14	e58d3010	str	r3, [sp, #16]
00006e18	e1a00002	mov	r0, r2
00006e1c	eb00112e	bl	0xb2dc	; symbol stub for: __ZN6google8protobuf7Message9MergeFromERKS1_
00006e20	ea00001b	b	0x6e94
00006e24	e59d3010	ldr	r3, [sp, #16]
00006e28	e59d2014	ldr	r2, [sp, #20]
00006e2c	e58d2000	str	r2, [sp]
00006e30	e3530001	cmp	r3, #1	; 0x1
00006e34	0a000009	beq	0x6e60
00006e38	e59d3000	ldr	r3, [sp]
00006e3c	e58d3004	str	r3, [sp, #4]
00006e40	e59d3044	ldr	r3, [sp, #68]
00006e44	e2832004	add	r2, r3, #4	; 0x4
00006e48	e3a03000	mov	r3, #0	; 0x0
00006e4c	e58d3010	str	r3, [sp, #16]
00006e50	e1a00002	mov	r0, r2
00006e54	eb00111d	bl	0xb2d0	; symbol stub for: __ZN6google8protobuf15UnknownFieldSetD1Ev
00006e58	e59d2004	ldr	r2, [sp, #4]
00006e5c	e58d2000	str	r2, [sp]
00006e60	e59d3000	ldr	r3, [sp]
00006e64	e58d3008	str	r3, [sp, #8]
00006e68	e59d2044	ldr	r2, [sp, #68]
00006e6c	e3a03000	mov	r3, #0	; 0x0
00006e70	e58d3010	str	r3, [sp, #16]
00006e74	e1a00002	mov	r0, r2
00006e78	eb00111a	bl	0xb2e8	; symbol stub for: __ZN6google8protobuf7MessageD2Ev
00006e7c	e59d2008	ldr	r2, [sp, #8]
00006e80	e58d2000	str	r2, [sp]
00006e84	e3e03000	mvn	r3, #0	; 0x0
00006e88	e58d3010	str	r3, [sp, #16]
00006e8c	e59d0000	ldr	r0, [sp]
00006e90	eb0010f9	bl	0xb27c	; symbol stub for: __Unwind_SjLj_Resume
00006e94	e28d300c	add	r3, sp, #12	; 0xc
00006e98	e1a00003	mov	r0, r3
00006e9c	eb0010f9	bl	0xb288	; symbol stub for: __Unwind_SjLj_Unregister
00006ea0	e247d05c	sub	sp, r7, #92	; 0x5c
00006ea4	ecbd8b11	fldmiax	sp!, {d8-d15}
00006ea8	e247d018	sub	sp, r7, #24	; 0x18
00006eac	e8bd0d00	ldmia	sp!, {r8, r10, r11}
00006eb0	e8bd80f0	ldmia	sp!, {r4, r5, r6, r7, pc}
00006eb4	000052d0	ldreqd	r5, [r0], -r0
00006eb8	00005890	muleq	r0, r0, r8
00006ebc	000000b0	streqh	r0, [r0], -r0
00006ec0	00005290	muleq	r0, r0, r2
00006ec4	000055f0	streqd	r5, [r0], -r0
00006ec8	00000000	andeq	r0, r0, r0
__ZN3dsp21MicrophoneDescriptionC1ERKS0_:
00006ecc	e92d40f0	stmdb	sp!, {r4, r5, r6, r7, lr}
00006ed0	e28d700c	add	r7, sp, #12	; 0xc
00006ed4	e92d0d00	stmdb	sp!, {r8, r10, r11}
00006ed8	ed2d8b11	fstmdbx	sp!, {d8-d15}
00006edc	e24dd048	sub	sp, sp, #72	; 0x48
00006ee0	e58d0044	str	r0, [sp, #68]
00006ee4	e58d1040	str	r1, [sp, #64]
00006ee8	e59f3168	ldr	r3, [pc, #360]	; 0x7058
00006eec	e08f3003	add	r3, pc, r3
00006ef0	e5933000	ldr	r3, [r3]
00006ef4	e58d3024	str	r3, [sp, #36]
00006ef8	e59f315c	ldr	r3, [pc, #348]	; 0x705c
00006efc	e08f3003	add	r3, pc, r3
00006f00	e58d3028	str	r3, [sp, #40]
00006f04	e28d202c	add	r2, sp, #44	; 0x2c
00006f08	e5827000	str	r7, [r2]
00006f0c	e59f314c	ldr	r3, [pc, #332]	; 0x7060
00006f10	e08f3003	add	r3, pc, r3
00006f14	e5823004	str	r3, [r2, #4]
00006f18	e582d008	str	sp, [r2, #8]
00006f1c	e28d300c	add	r3, sp, #12	; 0xc
00006f20	e1a00003	mov	r0, r3
00006f24	eb0010d1	bl	0xb270	; symbol stub for: __Unwind_SjLj_Register
00006f28	e59d3044	ldr	r3, [sp, #68]
00006f2c	e1a00003	mov	r0, r3
00006f30	e59f312c	ldr	r3, [pc, #300]	; 0x7064
00006f34	e08f3003	add	r3, pc, r3
00006f38	e5933000	ldr	r3, [r3]
00006f3c	e12fff33	blx	r3
00006f40	e59f3120	ldr	r3, [pc, #288]	; 0x7068
00006f44	e08f3003	add	r3, pc, r3
00006f48	e2832008	add	r2, r3, #8	; 0x8
00006f4c	e59d3044	ldr	r3, [sp, #68]
00006f50	e5832000	str	r2, [r3]
00006f54	e59d3044	ldr	r3, [sp, #68]
00006f58	e2832004	add	r2, r3, #4	; 0x4
00006f5c	e3a03002	mov	r3, #2	; 0x2
00006f60	e58d3010	str	r3, [sp, #16]
00006f64	e1a00002	mov	r0, r2
00006f68	eb0010d5	bl	0xb2c4	; symbol stub for: __ZN6google8protobuf15UnknownFieldSetC1Ev
00006f6c	e59d2044	ldr	r2, [sp, #68]
00006f70	e3a03000	mov	r3, #0	; 0x0
00006f74	e5823008	str	r3, [r2, #8]
00006f78	e59d2044	ldr	r2, [sp, #68]
00006f7c	e59f30e8	ldr	r3, [pc, #232]	; 0x706c
00006f80	e582300c	str	r3, [r2, #12]
00006f84	e59d2044	ldr	r2, [sp, #68]
00006f88	e3a03000	mov	r3, #0	; 0x0
00006f8c	e5823010	str	r3, [r2, #16]
00006f90	e59d2044	ldr	r2, [sp, #68]
00006f94	e3a03000	mov	r3, #0	; 0x0
00006f98	e5823014	str	r3, [r2, #20]
00006f9c	e59d3044	ldr	r3, [sp, #68]
00006fa0	e2832018	add	r2, r3, #24	; 0x18
00006fa4	e3a03000	mov	r3, #0	; 0x0
00006fa8	e5823000	str	r3, [r2]
00006fac	e59d2044	ldr	r2, [sp, #68]
00006fb0	e59d1040	ldr	r1, [sp, #64]
00006fb4	e3a03001	mov	r3, #1	; 0x1
00006fb8	e58d3010	str	r3, [sp, #16]
00006fbc	e1a00002	mov	r0, r2
00006fc0	eb0010c5	bl	0xb2dc	; symbol stub for: __ZN6google8protobuf7Message9MergeFromERKS1_
00006fc4	ea00001b	b	0x7038
00006fc8	e59d3010	ldr	r3, [sp, #16]
00006fcc	e59d2014	ldr	r2, [sp, #20]
00006fd0	e58d2000	str	r2, [sp]
00006fd4	e3530001	cmp	r3, #1	; 0x1
00006fd8	0a000009	beq	0x7004
00006fdc	e59d3000	ldr	r3, [sp]
00006fe0	e58d3004	str	r3, [sp, #4]
00006fe4	e59d3044	ldr	r3, [sp, #68]
00006fe8	e2832004	add	r2, r3, #4	; 0x4
00006fec	e3a03000	mov	r3, #0	; 0x0
00006ff0	e58d3010	str	r3, [sp, #16]
00006ff4	e1a00002	mov	r0, r2
00006ff8	eb0010b4	bl	0xb2d0	; symbol stub for: __ZN6google8protobuf15UnknownFieldSetD1Ev
00006ffc	e59d2004	ldr	r2, [sp, #4]
00007000	e58d2000	str	r2, [sp]
00007004	e59d3000	ldr	r3, [sp]
00007008	e58d3008	str	r3, [sp, #8]
0000700c	e59d2044	ldr	r2, [sp, #68]
00007010	e3a03000	mov	r3, #0	; 0x0
00007014	e58d3010	str	r3, [sp, #16]
00007018	e1a00002	mov	r0, r2
0000701c	eb0010b1	bl	0xb2e8	; symbol stub for: __ZN6google8protobuf7MessageD2Ev
00007020	e59d2008	ldr	r2, [sp, #8]
00007024	e58d2000	str	r2, [sp]
00007028	e3e03000	mvn	r3, #0	; 0x0
0000702c	e58d3010	str	r3, [sp, #16]
00007030	e59d0000	ldr	r0, [sp]
00007034	eb001090	bl	0xb27c	; symbol stub for: __Unwind_SjLj_Resume
00007038	e28d300c	add	r3, sp, #12	; 0xc
0000703c	e1a00003	mov	r0, r3
00007040	eb001090	bl	0xb288	; symbol stub for: __Unwind_SjLj_Unregister
00007044	e247d05c	sub	sp, r7, #92	; 0x5c
00007048	ecbd8b11	fldmiax	sp!, {d8-d15}
0000704c	e247d018	sub	sp, r7, #24	; 0x18
00007050	e8bd0d00	ldmia	sp!, {r8, r10, r11}
00007054	e8bd80f0	ldmia	sp!, {r4, r5, r6, r7, pc}
00007058	0000512c	andeq	r5, r0, ip, lsr #2
0000705c	000056f4	streqd	r5, [r0], -r4
00007060	000000b0	streqh	r0, [r0], -r0
00007064	000050ec	andeq	r5, r0, ip, ror #1
00007068	0000544c	andeq	r5, r0, ip, asr #8
0000706c	00000000	andeq	r0, r0, r0
__ZN3dsp21MicrophoneDescriptionD2Ev:
00007070	e92d40f0	stmdb	sp!, {r4, r5, r6, r7, lr}
00007074	e28d700c	add	r7, sp, #12	; 0xc
00007078	e92d0d00	stmdb	sp!, {r8, r10, r11}
0000707c	ed2d8b11	fstmdbx	sp!, {d8-d15}
00007080	e24dd044	sub	sp, sp, #68	; 0x44
00007084	e58d0040	str	r0, [sp, #64]
00007088	e59f30f8	ldr	r3, [pc, #248]	; 0x7188
0000708c	e08f3003	add	r3, pc, r3
00007090	e5933000	ldr	r3, [r3]
00007094	e58d3024	str	r3, [sp, #36]
00007098	e59f30ec	ldr	r3, [pc, #236]	; 0x718c
0000709c	e08f3003	add	r3, pc, r3
000070a0	e58d3028	str	r3, [sp, #40]
000070a4	e28d202c	add	r2, sp, #44	; 0x2c
000070a8	e5827000	str	r7, [r2]
000070ac	e59f30dc	ldr	r3, [pc, #220]	; 0x7190
000070b0	e08f3003	add	r3, pc, r3
000070b4	e5823004	str	r3, [r2, #4]
000070b8	e582d008	str	sp, [r2, #8]
000070bc	e28d300c	add	r3, sp, #12	; 0xc
000070c0	e1a00003	mov	r0, r3
000070c4	eb001069	bl	0xb270	; symbol stub for: __Unwind_SjLj_Register
000070c8	e59f30c4	ldr	r3, [pc, #196]	; 0x7194
000070cc	e08f3003	add	r3, pc, r3
000070d0	e2832008	add	r2, r3, #8	; 0x8
000070d4	e59d3040	ldr	r3, [sp, #64]
000070d8	e5832000	str	r2, [r3]
000070dc	e59d3040	ldr	r3, [sp, #64]
000070e0	e2832004	add	r2, r3, #4	; 0x4
000070e4	e3a03001	mov	r3, #1	; 0x1
000070e8	e58d3010	str	r3, [sp, #16]
000070ec	e1a00002	mov	r0, r2
000070f0	eb001076	bl	0xb2d0	; symbol stub for: __ZN6google8protobuf15UnknownFieldSetD1Ev
000070f4	e59d3040	ldr	r3, [sp, #64]
000070f8	e58d3004	str	r3, [sp, #4]
000070fc	e3e03000	mvn	r3, #0	; 0x0
00007100	e58d3010	str	r3, [sp, #16]
00007104	e59d0004	ldr	r0, [sp, #4]
00007108	eb001076	bl	0xb2e8	; symbol stub for: __ZN6google8protobuf7MessageD2Ev
0000710c	e3a03000	mov	r3, #0	; 0x0
00007110	e6ef3073	uxtb r3,r3
00007114	e3530000	cmp	r3, #0	; 0x0
00007118	0a000012	beq	0x7168
0000711c	ea00000f	b	0x7160
00007120	e59d3014	ldr	r3, [sp, #20]
00007124	e58d3000	str	r3, [sp]
00007128	e59d3000	ldr	r3, [sp]
0000712c	e58d3008	str	r3, [sp, #8]
00007130	e59d3040	ldr	r3, [sp, #64]
00007134	e58d3004	str	r3, [sp, #4]
00007138	e3a03000	mov	r3, #0	; 0x0
0000713c	e58d3010	str	r3, [sp, #16]
00007140	e59d0004	ldr	r0, [sp, #4]
00007144	eb001067	bl	0xb2e8	; symbol stub for: __ZN6google8protobuf7MessageD2Ev
00007148	e59d3008	ldr	r3, [sp, #8]
0000714c	e58d3000	str	r3, [sp]
00007150	e3e03000	mvn	r3, #0	; 0x0
00007154	e58d3010	str	r3, [sp, #16]
00007158	e59d0000	ldr	r0, [sp]
0000715c	eb001046	bl	0xb27c	; symbol stub for: __Unwind_SjLj_Resume
00007160	e59d0040	ldr	r0, [sp, #64]
00007164	eb001077	bl	0xb348	; symbol stub for: __ZdlPv
00007168	e28d300c	add	r3, sp, #12	; 0xc
0000716c	e1a00003	mov	r0, r3
00007170	eb001044	bl	0xb288	; symbol stub for: __Unwind_SjLj_Unregister
00007174	e247d05c	sub	sp, r7, #92	; 0x5c
00007178	ecbd8b11	fldmiax	sp!, {d8-d15}
0000717c	e247d018	sub	sp, r7, #24	; 0x18
00007180	e8bd0d00	ldmia	sp!, {r8, r10, r11}
00007184	e8bd80f0	ldmia	sp!, {r4, r5, r6, r7, pc}
00007188	00004f8c	andeq	r4, r0, ip, lsl #31
0000718c	0000555c	andeq	r5, r0, ip, asr r5
00007190	00000068	andeq	r0, r0, r8, rrx
00007194	000052c4	andeq	r5, r0, r4, asr #5
__ZN3dsp21MicrophoneDescriptionD1Ev:
00007198	e92d40f0	stmdb	sp!, {r4, r5, r6, r7, lr}
0000719c	e28d700c	add	r7, sp, #12	; 0xc
000071a0	e92d0d00	stmdb	sp!, {r8, r10, r11}
000071a4	ed2d8b11	fstmdbx	sp!, {d8-d15}
000071a8	e24dd044	sub	sp, sp, #68	; 0x44
000071ac	e58d0040	str	r0, [sp, #64]
000071b0	e59f30f8	ldr	r3, [pc, #248]	; 0x72b0
000071b4	e08f3003	add	r3, pc, r3
000071b8	e5933000	ldr	r3, [r3]
000071bc	e58d3024	str	r3, [sp, #36]
000071c0	e59f30ec	ldr	r3, [pc, #236]	; 0x72b4
000071c4	e08f3003	add	r3, pc, r3
000071c8	e58d3028	str	r3, [sp, #40]
000071cc	e28d202c	add	r2, sp, #44	; 0x2c
000071d0	e5827000	str	r7, [r2]
000071d4	e59f30dc	ldr	r3, [pc, #220]	; 0x72b8
000071d8	e08f3003	add	r3, pc, r3
000071dc	e5823004	str	r3, [r2, #4]
000071e0	e582d008	str	sp, [r2, #8]
000071e4	e28d300c	add	r3, sp, #12	; 0xc
000071e8	e1a00003	mov	r0, r3
000071ec	eb00101f	bl	0xb270	; symbol stub for: __Unwind_SjLj_Register
000071f0	e59f30c4	ldr	r3, [pc, #196]	; 0x72bc
000071f4	e08f3003	add	r3, pc, r3
000071f8	e2832008	add	r2, r3, #8	; 0x8
000071fc	e59d3040	ldr	r3, [sp, #64]
00007200	e5832000	str	r2, [r3]
00007204	e59d3040	ldr	r3, [sp, #64]
00007208	e2832004	add	r2, r3, #4	; 0x4
0000720c	e3a03001	mov	r3, #1	; 0x1
00007210	e58d3010	str	r3, [sp, #16]
00007214	e1a00002	mov	r0, r2
00007218	eb00102c	bl	0xb2d0	; symbol stub for: __ZN6google8protobuf15UnknownFieldSetD1Ev
0000721c	e59d3040	ldr	r3, [sp, #64]
00007220	e58d3004	str	r3, [sp, #4]
00007224	e3e03000	mvn	r3, #0	; 0x0
00007228	e58d3010	str	r3, [sp, #16]
0000722c	e59d0004	ldr	r0, [sp, #4]
00007230	eb00102c	bl	0xb2e8	; symbol stub for: __ZN6google8protobuf7MessageD2Ev
00007234	e3a03000	mov	r3, #0	; 0x0
00007238	e6ef3073	uxtb r3,r3
0000723c	e3530000	cmp	r3, #0	; 0x0
00007240	0a000012	beq	0x7290
00007244	ea00000f	b	0x7288
00007248	e59d3014	ldr	r3, [sp, #20]
0000724c	e58d3000	str	r3, [sp]
00007250	e59d3000	ldr	r3, [sp]
00007254	e58d3008	str	r3, [sp, #8]
00007258	e59d3040	ldr	r3, [sp, #64]
0000725c	e58d3004	str	r3, [sp, #4]
00007260	e3a03000	mov	r3, #0	; 0x0
00007264	e58d3010	str	r3, [sp, #16]
00007268	e59d0004	ldr	r0, [sp, #4]
0000726c	eb00101d	bl	0xb2e8	; symbol stub for: __ZN6google8protobuf7MessageD2Ev
00007270	e59d3008	ldr	r3, [sp, #8]
00007274	e58d3000	str	r3, [sp]
00007278	e3e03000	mvn	r3, #0	; 0x0
0000727c	e58d3010	str	r3, [sp, #16]
00007280	e59d0000	ldr	r0, [sp]
00007284	eb000ffc	bl	0xb27c	; symbol stub for: __Unwind_SjLj_Resume
00007288	e59d0040	ldr	r0, [sp, #64]
0000728c	eb00102d	bl	0xb348	; symbol stub for: __ZdlPv
00007290	e28d300c	add	r3, sp, #12	; 0xc
00007294	e1a00003	mov	r0, r3
00007298	eb000ffa	bl	0xb288	; symbol stub for: __Unwind_SjLj_Unregister
0000729c	e247d05c	sub	sp, r7, #92	; 0x5c
000072a0	ecbd8b11	fldmiax	sp!, {d8-d15}
000072a4	e247d018	sub	sp, r7, #24	; 0x18
000072a8	e8bd0d00	ldmia	sp!, {r8, r10, r11}
000072ac	e8bd80f0	ldmia	sp!, {r4, r5, r6, r7, pc}
000072b0	00004e64	andeq	r4, r0, r4, ror #28
000072b4	0000543a	andeq	r5, r0, r10, lsr r4
000072b8	00000068	andeq	r0, r0, r8, rrx
000072bc	0000519c	muleq	r0, ip, r1
__ZN3dsp21MicrophoneDescriptionD0Ev:
000072c0	e92d40f0	stmdb	sp!, {r4, r5, r6, r7, lr}
000072c4	e28d700c	add	r7, sp, #12	; 0xc
000072c8	e92d0d00	stmdb	sp!, {r8, r10, r11}
000072cc	ed2d8b11	fstmdbx	sp!, {d8-d15}
000072d0	e24dd044	sub	sp, sp, #68	; 0x44
000072d4	e58d0040	str	r0, [sp, #64]
000072d8	e59f30f8	ldr	r3, [pc, #248]	; 0x73d8
000072dc	e08f3003	add	r3, pc, r3
000072e0	e5933000	ldr	r3, [r3]
000072e4	e58d3024	str	r3, [sp, #36]
000072e8	e59f30ec	ldr	r3, [pc, #236]	; 0x73dc
000072ec	e08f3003	add	r3, pc, r3
000072f0	e58d3028	str	r3, [sp, #40]
000072f4	e28d202c	add	r2, sp, #44	; 0x2c
000072f8	e5827000	str	r7, [r2]
000072fc	e59f30dc	ldr	r3, [pc, #220]	; 0x73e0
00007300	e08f3003	add	r3, pc, r3
00007304	e5823004	str	r3, [r2, #4]
00007308	e582d008	str	sp, [r2, #8]
0000730c	e28d300c	add	r3, sp, #12	; 0xc
00007310	e1a00003	mov	r0, r3
00007314	eb000fd5	bl	0xb270	; symbol stub for: __Unwind_SjLj_Register
00007318	e59f30c4	ldr	r3, [pc, #196]	; 0x73e4
0000731c	e08f3003	add	r3, pc, r3
00007320	e2832008	add	r2, r3, #8	; 0x8
00007324	e59d3040	ldr	r3, [sp, #64]
00007328	e5832000	str	r2, [r3]
0000732c	e59d3040	ldr	r3, [sp, #64]
00007330	e2832004	add	r2, r3, #4	; 0x4
00007334	e3a03001	mov	r3, #1	; 0x1
00007338	e58d3010	str	r3, [sp, #16]
0000733c	e1a00002	mov	r0, r2
00007340	eb000fe2	bl	0xb2d0	; symbol stub for: __ZN6google8protobuf15UnknownFieldSetD1Ev
00007344	e59d3040	ldr	r3, [sp, #64]
00007348	e58d3004	str	r3, [sp, #4]
0000734c	e3e03000	mvn	r3, #0	; 0x0
00007350	e58d3010	str	r3, [sp, #16]
00007354	e59d0004	ldr	r0, [sp, #4]
00007358	eb000fe2	bl	0xb2e8	; symbol stub for: __ZN6google8protobuf7MessageD2Ev
0000735c	e3a03001	mov	r3, #1	; 0x1
00007360	e6ef3073	uxtb r3,r3
00007364	e3530000	cmp	r3, #0	; 0x0
00007368	0a000012	beq	0x73b8
0000736c	ea00000f	b	0x73b0
00007370	e59d3014	ldr	r3, [sp, #20]
00007374	e58d3000	str	r3, [sp]
00007378	e59d3000	ldr	r3, [sp]
0000737c	e58d3008	str	r3, [sp, #8]
00007380	e59d3040	ldr	r3, [sp, #64]
00007384	e58d3004	str	r3, [sp, #4]
00007388	e3a03000	mov	r3, #0	; 0x0
0000738c	e58d3010	str	r3, [sp, #16]
00007390	e59d0004	ldr	r0, [sp, #4]
00007394	eb000fd3	bl	0xb2e8	; symbol stub for: __ZN6google8protobuf7MessageD2Ev
00007398	e59d3008	ldr	r3, [sp, #8]
0000739c	e58d3000	str	r3, [sp]
000073a0	e3e03000	mvn	r3, #0	; 0x0
000073a4	e58d3010	str	r3, [sp, #16]
000073a8	e59d0000	ldr	r0, [sp]
000073ac	eb000fb2	bl	0xb27c	; symbol stub for: __Unwind_SjLj_Resume
000073b0	e59d0040	ldr	r0, [sp, #64]
000073b4	eb000fe3	bl	0xb348	; symbol stub for: __ZdlPv
000073b8	e28d300c	add	r3, sp, #12	; 0xc
000073bc	e1a00003	mov	r0, r3
000073c0	eb000fb0	bl	0xb288	; symbol stub for: __Unwind_SjLj_Unregister
000073c4	e247d05c	sub	sp, r7, #92	; 0x5c
000073c8	ecbd8b11	fldmiax	sp!, {d8-d15}
000073cc	e247d018	sub	sp, r7, #24	; 0x18
000073d0	e8bd0d00	ldmia	sp!, {r8, r10, r11}
000073d4	e8bd80f0	ldmia	sp!, {r4, r5, r6, r7, pc}
000073d8	00004d3c	andeq	r4, r0, ip, lsr sp
000073dc	00005318	andeq	r5, r0, r8, lsl r3
000073e0	00000068	andeq	r0, r0, r8, rrx
000073e4	00005074	andeq	r5, r0, r4, ror r0
__ZN3dsp21MicrophoneDescription10descriptorEv:
000073e8	e92d4080	stmdb	sp!, {r7, lr}
000073ec	e28d7000	add	r7, sp, #0	; 0x0
000073f0	e59f3024	ldr	r3, [pc, #36]	; 0x741c
000073f4	e08f3003	add	r3, pc, r3
000073f8	e5933000	ldr	r3, [r3]
000073fc	e3530000	cmp	r3, #0	; 0x0
00007400	1a000000	bne	0x7408
00007404	ebffeff9	bl	__ZN3dsp30protobuf_BuildDesc_dsp_2eprotoEv
00007408	e59f3010	ldr	r3, [pc, #16]	; 0x7420
0000740c	e08f3003	add	r3, pc, r3
00007410	e5933000	ldr	r3, [r3]
00007414	e1a00003	mov	r0, r3
00007418	e8bd8080	ldmia	sp!, {r7, pc}
0000741c	00004d0c	andeq	r4, r0, ip, lsl #26
00007420	00004cf4	streqd	r4, [r0], -r4
__ZN3dsp21MicrophoneDescription16default_instanceEv:
00007424	e92d4080	stmdb	sp!, {r7, lr}
00007428	e28d7000	add	r7, sp, #0	; 0x0
0000742c	e59f3024	ldr	r3, [pc, #36]	; 0x7458
00007430	e08f3003	add	r3, pc, r3
00007434	e5933000	ldr	r3, [r3]
00007438	e3530000	cmp	r3, #0	; 0x0
0000743c	1a000000	bne	0x7444
00007440	ebffefea	bl	__ZN3dsp30protobuf_BuildDesc_dsp_2eprotoEv
00007444	e59f3010	ldr	r3, [pc, #16]	; 0x745c
00007448	e08f3003	add	r3, pc, r3
0000744c	e5933000	ldr	r3, [r3]
00007450	e1a00003	mov	r0, r3
00007454	e8bd8080	ldmia	sp!, {r7, pc}
00007458	00004ca0	andeq	r4, r0, r0, lsr #25
0000745c	00004c88	andeq	r4, r0, r8, lsl #25
__ZNK3dsp21MicrophoneDescription3NewEv:
00007460	e92d40f0	stmdb	sp!, {r4, r5, r6, r7, lr}
00007464	e28d700c	add	r7, sp, #12	; 0xc
00007468	e92d0d00	stmdb	sp!, {r8, r10, r11}
0000746c	ed2d8b11	fstmdbx	sp!, {d8-d15}
00007470	e24dd048	sub	sp, sp, #72	; 0x48
00007474	e58d0044	str	r0, [sp, #68]
00007478	e59f30c0	ldr	r3, [pc, #192]	; 0x7540
0000747c	e08f3003	add	r3, pc, r3
00007480	e5933000	ldr	r3, [r3]
00007484	e58d3028	str	r3, [sp, #40]
00007488	e59f30b4	ldr	r3, [pc, #180]	; 0x7544
0000748c	e08f3003	add	r3, pc, r3
00007490	e58d302c	str	r3, [sp, #44]
00007494	e28d2030	add	r2, sp, #48	; 0x30
00007498	e5827000	str	r7, [r2]
0000749c	e59f30a4	ldr	r3, [pc, #164]	; 0x7548
000074a0	e08f3003	add	r3, pc, r3
000074a4	e5823004	str	r3, [r2, #4]
000074a8	e582d008	str	sp, [r2, #8]
000074ac	e28d3010	add	r3, sp, #16	; 0x10
000074b0	e1a00003	mov	r0, r3
000074b4	eb000f6d	bl	0xb270	; symbol stub for: __Unwind_SjLj_Register
000074b8	e3e03000	mvn	r3, #0	; 0x0
000074bc	e58d3014	str	r3, [sp, #20]
000074c0	e3a0001c	mov	r0, #28	; 0x1c
000074c4	eb000fa2	bl	0xb354	; symbol stub for: __Znwm
000074c8	e1a03000	mov	r3, r0
000074cc	e58d3008	str	r3, [sp, #8]
000074d0	e3a03001	mov	r3, #1	; 0x1
000074d4	e58d3014	str	r3, [sp, #20]
000074d8	e59d0008	ldr	r0, [sp, #8]
000074dc	ebfff118	bl	__ZN3dsp21MicrophoneDescriptionC1Ev
000074e0	ea00000b	b	0x7514
000074e4	e59d3018	ldr	r3, [sp, #24]
000074e8	e58d3000	str	r3, [sp]
000074ec	e59d3000	ldr	r3, [sp]
000074f0	e58d300c	str	r3, [sp, #12]
000074f4	e59d0008	ldr	r0, [sp, #8]
000074f8	eb000f92	bl	0xb348	; symbol stub for: __ZdlPv
000074fc	e59d300c	ldr	r3, [sp, #12]
00007500	e58d3000	str	r3, [sp]
00007504	e3e03000	mvn	r3, #0	; 0x0
00007508	e58d3014	str	r3, [sp, #20]
0000750c	e59d0000	ldr	r0, [sp]
00007510	eb000f59	bl	0xb27c	; symbol stub for: __Unwind_SjLj_Resume
00007514	e59d3008	ldr	r3, [sp, #8]
00007518	e58d3004	str	r3, [sp, #4]
0000751c	e28d3010	add	r3, sp, #16	; 0x10
00007520	e1a00003	mov	r0, r3
00007524	eb000f57	bl	0xb288	; symbol stub for: __Unwind_SjLj_Unregister
00007528	e59d0004	ldr	r0, [sp, #4]
0000752c	e247d05c	sub	sp, r7, #92	; 0x5c
00007530	ecbd8b11	fldmiax	sp!, {d8-d15}
00007534	e247d018	sub	sp, r7, #24	; 0x18
00007538	e8bd0d00	ldmia	sp!, {r8, r10, r11}
0000753c	e8bd80f0	ldmia	sp!, {r4, r5, r6, r7, pc}
00007540	00004b9c	muleq	r0, ip, r11
00007544	0000517e	andeq	r5, r0, lr, ror r1
00007548	0000003c	andeq	r0, r0, ip, lsr r0
__ZNK3dsp21MicrophoneDescription13GetDescriptorEv:
0000754c	e92d4080	stmdb	sp!, {r7, lr}
00007550	e28d7000	add	r7, sp, #0	; 0x0
00007554	e24dd004	sub	sp, sp, #4	; 0x4
00007558	e58d0000	str	r0, [sp]
0000755c	ebffffa1	bl	__ZN3dsp21MicrophoneDescription10descriptorEv
00007560	e1a03000	mov	r3, r0
00007564	e1a00003	mov	r0, r3
00007568	e247d000	sub	sp, r7, #0	; 0x0
0000756c	e8bd8080	ldmia	sp!, {r7, pc}
__ZNK3dsp21MicrophoneDescription13GetReflectionEv:
00007570	e92d4080	stmdb	sp!, {r7, lr}
00007574	e28d7000	add	r7, sp, #0	; 0x0
00007578	e24dd004	sub	sp, sp, #4	; 0x4
0000757c	e58d0000	str	r0, [sp]
00007580	e59f3028	ldr	r3, [pc, #40]	; 0x75b0
00007584	e08f3003	add	r3, pc, r3
00007588	e5933000	ldr	r3, [r3]
0000758c	e3530000	cmp	r3, #0	; 0x0
00007590	1a000000	bne	0x7598
00007594	ebffef95	bl	__ZN3dsp30protobuf_BuildDesc_dsp_2eprotoEv
00007598	e59f3014	ldr	r3, [pc, #20]	; 0x75b4
0000759c	e08f3003	add	r3, pc, r3
000075a0	e5933000	ldr	r3, [r3]
000075a4	e1a00003	mov	r0, r3
000075a8	e247d000	sub	sp, r7, #0	; 0x0
000075ac	e8bd8080	ldmia	sp!, {r7, pc}
000075b0	00004b78	andeq	r4, r0, r8, ror r11
000075b4	00004b60	andeq	r4, r0, r0, ror #22
__ZN3dsp17MicrophoneMessageC2Ev:
000075b8	e92d40f0	stmdb	sp!, {r4, r5, r6, r7, lr}
000075bc	e28d700c	add	r7, sp, #12	; 0xc
000075c0	e92d0d00	stmdb	sp!, {r8, r10, r11}
000075c4	ed2d8b11	fstmdbx	sp!, {d8-d15}
000075c8	e24dd040	sub	sp, sp, #64	; 0x40
000075cc	e58d003c	str	r0, [sp, #60]
000075d0	e59f311c	ldr	r3, [pc, #284]	; 0x76f4
000075d4	e08f3003	add	r3, pc, r3
000075d8	e5933000	ldr	r3, [r3]
000075dc	e58d3020	str	r3, [sp, #32]
000075e0	e59f3110	ldr	r3, [pc, #272]	; 0x76f8
000075e4	e08f3003	add	r3, pc, r3
000075e8	e58d3024	str	r3, [sp, #36]
000075ec	e28d2028	add	r2, sp, #40	; 0x28
000075f0	e5827000	str	r7, [r2]
000075f4	e59f3100	ldr	r3, [pc, #256]	; 0x76fc
000075f8	e08f3003	add	r3, pc, r3
000075fc	e5823004	str	r3, [r2, #4]
00007600	e582d008	str	sp, [r2, #8]
00007604	e28d3008	add	r3, sp, #8	; 0x8
00007608	e1a00003	mov	r0, r3
0000760c	eb000f17	bl	0xb270	; symbol stub for: __Unwind_SjLj_Register
00007610	e59d303c	ldr	r3, [sp, #60]
00007614	e1a00003	mov	r0, r3
00007618	e59f30e0	ldr	r3, [pc, #224]	; 0x7700
0000761c	e08f3003	add	r3, pc, r3
00007620	e5933000	ldr	r3, [r3]
00007624	e12fff33	blx	r3
00007628	e59f30d4	ldr	r3, [pc, #212]	; 0x7704
0000762c	e08f3003	add	r3, pc, r3
00007630	e2832008	add	r2, r3, #8	; 0x8
00007634	e59d303c	ldr	r3, [sp, #60]
00007638	e5832000	str	r2, [r3]
0000763c	e59d303c	ldr	r3, [sp, #60]
00007640	e2832004	add	r2, r3, #4	; 0x4
00007644	e3a03001	mov	r3, #1	; 0x1
00007648	e58d300c	str	r3, [sp, #12]
0000764c	e1a00002	mov	r0, r2
00007650	eb000f1b	bl	0xb2c4	; symbol stub for: __ZN6google8protobuf15UnknownFieldSetC1Ev
00007654	e59d203c	ldr	r2, [sp, #60]
00007658	e3a03000	mov	r3, #0	; 0x0
0000765c	e5823008	str	r3, [r2, #8]
00007660	e59d103c	ldr	r1, [sp, #60]
00007664	e3a02000	mov	r2, #0	; 0x0
00007668	e3a03000	mov	r3, #0	; 0x0
0000766c	e581200c	str	r2, [r1, #12]
00007670	e5813010	str	r3, [r1, #16]
00007674	e59d203c	ldr	r2, [sp, #60]
00007678	e59f3088	ldr	r3, [pc, #136]	; 0x7708
0000767c	e08f3003	add	r3, pc, r3
00007680	e5823014	str	r3, [r2, #20]
00007684	e59d303c	ldr	r3, [sp, #60]
00007688	e2832018	add	r2, r3, #24	; 0x18
0000768c	e3a03000	mov	r3, #0	; 0x0
00007690	e5823000	str	r3, [r2]
00007694	ea00000e	b	0x76d4
00007698	e59d3010	ldr	r3, [sp, #16]
0000769c	e58d3000	str	r3, [sp]
000076a0	e59d3000	ldr	r3, [sp]
000076a4	e58d3004	str	r3, [sp, #4]
000076a8	e59d203c	ldr	r2, [sp, #60]
000076ac	e3a03000	mov	r3, #0	; 0x0
000076b0	e58d300c	str	r3, [sp, #12]
000076b4	e1a00002	mov	r0, r2
000076b8	eb000f0a	bl	0xb2e8	; symbol stub for: __ZN6google8protobuf7MessageD2Ev
000076bc	e59d3004	ldr	r3, [sp, #4]
000076c0	e58d3000	str	r3, [sp]
000076c4	e3e03000	mvn	r3, #0	; 0x0
000076c8	e58d300c	str	r3, [sp, #12]
000076cc	e59d0000	ldr	r0, [sp]
000076d0	eb000ee9	bl	0xb27c	; symbol stub for: __Unwind_SjLj_Resume
000076d4	e28d3008	add	r3, sp, #8	; 0x8
000076d8	e1a00003	mov	r0, r3
000076dc	eb000ee9	bl	0xb288	; symbol stub for: __Unwind_SjLj_Unregister
000076e0	e247d05c	sub	sp, r7, #92	; 0x5c
000076e4	ecbd8b11	fldmiax	sp!, {d8-d15}
000076e8	e247d018	sub	sp, r7, #24	; 0x18
000076ec	e8bd0d00	ldmia	sp!, {r8, r10, r11}
000076f0	e8bd80f0	ldmia	sp!, {r4, r5, r6, r7, pc}
000076f4	00004a44	andeq	r4, r0, r4, asr #20
000076f8	0000502c	andeq	r5, r0, ip, lsr #32
000076fc	00000098	muleq	r0, r8, r0
00007700	00004a04	andeq	r4, r0, r4, lsl #20
00007704	00004dac	andeq	r4, r0, ip, lsr #27
00007708	0000505c	andeq	r5, r0, ip, asr r0
__ZN3dsp17MicrophoneMessageC2ERKS0_:
0000770c	e92d40f0	stmdb	sp!, {r4, r5, r6, r7, lr}
00007710	e28d700c	add	r7, sp, #12	; 0xc
00007714	e92d0d00	stmdb	sp!, {r8, r10, r11}
00007718	ed2d8b11	fstmdbx	sp!, {d8-d15}
0000771c	e24dd048	sub	sp, sp, #72	; 0x48
00007720	e58d0044	str	r0, [sp, #68]
00007724	e58d1040	str	r1, [sp, #64]
00007728	e59f3168	ldr	r3, [pc, #360]	; 0x7898
0000772c	e08f3003	add	r3, pc, r3
00007730	e5933000	ldr	r3, [r3]
00007734	e58d3024	str	r3, [sp, #36]
00007738	e59f315c	ldr	r3, [pc, #348]	; 0x789c
0000773c	e08f3003	add	r3, pc, r3
00007740	e58d3028	str	r3, [sp, #40]
00007744	e28d202c	add	r2, sp, #44	; 0x2c
00007748	e5827000	str	r7, [r2]
0000774c	e59f314c	ldr	r3, [pc, #332]	; 0x78a0
00007750	e08f3003	add	r3, pc, r3
00007754	e5823004	str	r3, [r2, #4]
00007758	e582d008	str	sp, [r2, #8]
0000775c	e28d300c	add	r3, sp, #12	; 0xc
00007760	e1a00003	mov	r0, r3
00007764	eb000ec1	bl	0xb270	; symbol stub for: __Unwind_SjLj_Register
00007768	e59d3044	ldr	r3, [sp, #68]
0000776c	e1a00003	mov	r0, r3
00007770	e59f312c	ldr	r3, [pc, #300]	; 0x78a4
00007774	e08f3003	add	r3, pc, r3
00007778	e5933000	ldr	r3, [r3]
0000777c	e12fff33	blx	r3
00007780	e59f3120	ldr	r3, [pc, #288]	; 0x78a8
00007784	e08f3003	add	r3, pc, r3
00007788	e2832008	add	r2, r3, #8	; 0x8
0000778c	e59d3044	ldr	r3, [sp, #68]
00007790	e5832000	str	r2, [r3]
00007794	e59d3044	ldr	r3, [sp, #68]
00007798	e2832004	add	r2, r3, #4	; 0x4
0000779c	e3a03002	mov	r3, #2	; 0x2
000077a0	e58d3010	str	r3, [sp, #16]
000077a4	e1a00002	mov	r0, r2
000077a8	eb000ec5	bl	0xb2c4	; symbol stub for: __ZN6google8protobuf15UnknownFieldSetC1Ev
000077ac	e59d2044	ldr	r2, [sp, #68]
000077b0	e3a03000	mov	r3, #0	; 0x0
000077b4	e5823008	str	r3, [r2, #8]
000077b8	e59d1044	ldr	r1, [sp, #68]
000077bc	e3a02000	mov	r2, #0	; 0x0
000077c0	e3a03000	mov	r3, #0	; 0x0
000077c4	e581200c	str	r2, [r1, #12]
000077c8	e5813010	str	r3, [r1, #16]
000077cc	e59d2044	ldr	r2, [sp, #68]
000077d0	e59f30d4	ldr	r3, [pc, #212]	; 0x78ac
000077d4	e08f3003	add	r3, pc, r3
000077d8	e5823014	str	r3, [r2, #20]
000077dc	e59d3044	ldr	r3, [sp, #68]
000077e0	e2832018	add	r2, r3, #24	; 0x18
000077e4	e3a03000	mov	r3, #0	; 0x0
000077e8	e5823000	str	r3, [r2]
000077ec	e59d2044	ldr	r2, [sp, #68]
000077f0	e59d1040	ldr	r1, [sp, #64]
000077f4	e3a03001	mov	r3, #1	; 0x1
000077f8	e58d3010	str	r3, [sp, #16]
000077fc	e1a00002	mov	r0, r2
00007800	eb000eb5	bl	0xb2dc	; symbol stub for: __ZN6google8protobuf7Message9MergeFromERKS1_
00007804	ea00001b	b	0x7878
00007808	e59d3010	ldr	r3, [sp, #16]
0000780c	e59d2014	ldr	r2, [sp, #20]
00007810	e58d2000	str	r2, [sp]
00007814	e3530001	cmp	r3, #1	; 0x1
00007818	0a000009	beq	0x7844
0000781c	e59d3000	ldr	r3, [sp]
00007820	e58d3004	str	r3, [sp, #4]
00007824	e59d3044	ldr	r3, [sp, #68]
00007828	e2832004	add	r2, r3, #4	; 0x4
0000782c	e3a03000	mov	r3, #0	; 0x0
00007830	e58d3010	str	r3, [sp, #16]
00007834	e1a00002	mov	r0, r2
00007838	eb000ea4	bl	0xb2d0	; symbol stub for: __ZN6google8protobuf15UnknownFieldSetD1Ev
0000783c	e59d2004	ldr	r2, [sp, #4]
00007840	e58d2000	str	r2, [sp]
00007844	e59d3000	ldr	r3, [sp]
00007848	e58d3008	str	r3, [sp, #8]
0000784c	e59d2044	ldr	r2, [sp, #68]
00007850	e3a03000	mov	r3, #0	; 0x0
00007854	e58d3010	str	r3, [sp, #16]
00007858	e1a00002	mov	r0, r2
0000785c	eb000ea1	bl	0xb2e8	; symbol stub for: __ZN6google8protobuf7MessageD2Ev
00007860	e59d2008	ldr	r2, [sp, #8]
00007864	e58d2000	str	r2, [sp]
00007868	e3e03000	mvn	r3, #0	; 0x0
0000786c	e58d3010	str	r3, [sp, #16]
00007870		e59d0000	ldr	r0, [sp]
00007874	eb000e80	bl	0xb27c	; symbol stub for: __Unwind_SjLj_Resume
00007878	e28d300c	add	r3, sp, #12	; 0xc
0000787c	e1a00003	mov	r0, r3
00007880	eb000e80	bl	0xb288	; symbol stub for: __Unwind_SjLj_Unregister
00007884	e247d05c	sub	sp, r7, #92	; 0x5c
00007888	ecbd8b11	fldmiax	sp!, {d8-d15}
0000788c	e247d018	sub	sp, r7, #24	; 0x18
00007890	e8bd0d00	ldmia	sp!, {r8, r10, r11}
00007894	e8bd80f0	ldmia	sp!, {r4, r5, r6, r7, pc}
00007898	000048ec	andeq	r4, r0, ip, ror #17
0000789c	00004eda	ldreqd	r4, [r0], -r10
000078a0	000000b0	streqh	r0, [r0], -r0
000078a4	000048ac	andeq	r4, r0, ip, lsr #17
000078a8	00004c54	andeq	r4, r0, r4, asr ip
000078ac	00004f04	andeq	r4, r0, r4, lsl #30
__ZN3dsp17MicrophoneMessageC1ERKS0_:
000078b0	e92d40f0	stmdb	sp!, {r4, r5, r6, r7, lr}
000078b4	e28d700c	add	r7, sp, #12	; 0xc
000078b8	e92d0d00	stmdb	sp!, {r8, r10, r11}
000078bc	ed2d8b11	fstmdbx	sp!, {d8-d15}
000078c0	e24dd048	sub	sp, sp, #72	; 0x48
000078c4	e58d0044	str	r0, [sp, #68]
000078c8	e58d1040	str	r1, [sp, #64]
000078cc	e59f3168	ldr	r3, [pc, #360]	; 0x7a3c
000078d0	e08f3003	add	r3, pc, r3
000078d4	e5933000	ldr	r3, [r3]
000078d8	e58d3024	str	r3, [sp, #36]
000078dc	e59f315c	ldr	r3, [pc, #348]	; 0x7a40
000078e0	e08f3003	add	r3, pc, r3
000078e4	e58d3028	str	r3, [sp, #40]
000078e8	e28d202c	add	r2, sp, #44	; 0x2c
000078ec	e5827000	str	r7, [r2]
000078f0	e59f314c	ldr	r3, [pc, #332]	; 0x7a44
000078f4	e08f3003	add	r3, pc, r3
000078f8	e5823004	str	r3, [r2, #4]
000078fc	e582d008	str	sp, [r2, #8]
00007900	e28d300c	add	r3, sp, #12	; 0xc
00007904	e1a00003	mov	r0, r3
00007908	eb000e58	bl	0xb270	; symbol stub for: __Unwind_SjLj_Register
0000790c	e59d3044	ldr	r3, [sp, #68]
00007910	e1a00003	mov	r0, r3
00007914	e59f312c	ldr	r3, [pc, #300]	; 0x7a48
00007918	e08f3003	add	r3, pc, r3
0000791c	e5933000	ldr	r3, [r3]
00007920	e12fff33	blx	r3
00007924	e59f3120	ldr	r3, [pc, #288]	; 0x7a4c
00007928	e08f3003	add	r3, pc, r3
0000792c	e2832008	add	r2, r3, #8	; 0x8
00007930	e59d3044	ldr	r3, [sp, #68]
00007934	e5832000	str	r2, [r3]
00007938	e59d3044	ldr	r3, [sp, #68]
0000793c	e2832004	add	r2, r3, #4	; 0x4
00007940	e3a03002	mov	r3, #2	; 0x2
00007944	e58d3010	str	r3, [sp, #16]
00007948	e1a00002	mov	r0, r2
0000794c	eb000e5c	bl	0xb2c4	; symbol stub for: __ZN6google8protobuf15UnknownFieldSetC1Ev
00007950	e59d2044	ldr	r2, [sp, #68]
00007954	e3a03000	mov	r3, #0	; 0x0
00007958	e5823008	str	r3, [r2, #8]
0000795c	e59d1044	ldr	r1, [sp, #68]
00007960	e3a02000	mov	r2, #0	; 0x0
00007964	e3a03000	mov	r3, #0	; 0x0
00007968	e581200c	str	r2, [r1, #12]
0000796c	e5813010	str	r3, [r1, #16]
00007970	e59d2044	ldr	r2, [sp, #68]
00007974	e59f30d4	ldr	r3, [pc, #212]	; 0x7a50
00007978	e08f3003	add	r3, pc, r3
0000797c	e5823014	str	r3, [r2, #20]
00007980	e59d3044	ldr	r3, [sp, #68]
00007984	e2832018	add	r2, r3, #24	; 0x18
00007988	e3a03000	mov	r3, #0	; 0x0
0000798c	e5823000	str	r3, [r2]
00007990	e59d2044	ldr	r2, [sp, #68]
00007994	e59d1040	ldr	r1, [sp, #64]
00007998	e3a03001	mov	r3, #1	; 0x1
0000799c	e58d3010	str	r3, [sp, #16]
000079a0	e1a00002	mov	r0, r2
000079a4	eb000e4c	bl	0xb2dc	; symbol stub for: __ZN6google8protobuf7Message9MergeFromERKS1_
000079a8	ea00001b	b	0x7a1c
000079ac	e59d3010	ldr	r3, [sp, #16]
000079b0	e59d2014	ldr	r2, [sp, #20]
000079b4	e58d2000	str	r2, [sp]
000079b8	e3530001	cmp	r3, #1	; 0x1
000079bc	0a000009	beq	0x79e8
000079c0	e59d3000	ldr	r3, [sp]
000079c4	e58d3004	str	r3, [sp, #4]
000079c8	e59d3044	ldr	r3, [sp, #68]
000079cc	e2832004	add	r2, r3, #4	; 0x4
000079d0	e3a03000	mov	r3, #0	; 0x0
000079d4	e58d3010	str	r3, [sp, #16]
000079d8	e1a00002	mov	r0, r2
000079dc	eb000e3b	bl	0xb2d0	; symbol stub for: __ZN6google8protobuf15UnknownFieldSetD1Ev
000079e0	e59d2004	ldr	r2, [sp, #4]
000079e4	e58d2000	str	r2, [sp]
000079e8	e59d3000	ldr	r3, [sp]
000079ec	e58d3008	str	r3, [sp, #8]
000079f0	e59d2044	ldr	r2, [sp, #68]
000079f4	e3a03000	mov	r3, #0	; 0x0
000079f8	e58d3010	str	r3, [sp, #16]
000079fc	e1a00002	mov	r0, r2
00007a00	eb000e38	bl	0xb2e8	; symbol stub for: __ZN6google8protobuf7MessageD2Ev
00007a04	e59d2008	ldr	r2, [sp, #8]
00007a08	e58d2000	str	r2, [sp]
00007a0c	e3e03000	mvn	r3, #0	; 0x0
00007a10	e58d3010	str	r3, [sp, #16]
00007a14	e59d0000	ldr	r0, [sp]
00007a18	eb000e17	bl	0xb27c	; symbol stub for: __Unwind_SjLj_Resume
00007a1c	e28d300c	add	r3, sp, #12	; 0xc
00007a20	e1a00003	mov	r0, r3
00007a24	eb000e17	bl	0xb288	; symbol stub for: __Unwind_SjLj_Unregister
00007a28	e247d05c	sub	sp, r7, #92	; 0x5c
00007a2c	ecbd8b11	fldmiax	sp!, {d8-d15}
00007a30	e247d018	sub	sp, r7, #24	; 0x18
00007a34	e8bd0d00	ldmia	sp!, {r8, r10, r11}
00007a38	e8bd80f0	ldmia	sp!, {r4, r5, r6, r7, pc}
00007a3c	00004748	andeq	r4, r0, r8, asr #14
00007a40	00004d3e	andeq	r4, r0, lr, lsr sp
00007a44	000000b0	streqh	r0, [r0], -r0
00007a48	00004708	andeq	r4, r0, r8, lsl #14
00007a4c	00004ab0	streqh	r4, [r0], -r0
00007a50	00004d60	andeq	r4, r0, r0, ror #26
__ZN3dsp17MicrophoneMessageD2Ev:
00007a54	e92d40f0	stmdb	sp!, {r4, r5, r6, r7, lr}
00007a58	e28d700c	add	r7, sp, #12	; 0xc
00007a5c	e92d0d00	stmdb	sp!, {r8, r10, r11}
00007a60	ed2d8b11	fstmdbx	sp!, {d8-d15}
00007a64	e24dd050	sub	sp, sp, #80	; 0x50
00007a68	e58d004c	str	r0, [sp, #76]
00007a6c	e59f317c	ldr	r3, [pc, #380]	; 0x7bf0
00007a70	e08f3003	add	r3, pc, r3
00007a74	e5933000	ldr	r3, [r3]
00007a78	e58d3030	str	r3, [sp, #48]
00007a7c	e59f3170	ldr	r3, [pc, #368]	; 0x7bf4
00007a80	e08f3003	add	r3, pc, r3
00007a84	e58d3034	str	r3, [sp, #52]
00007a88	e28d2038	add	r2, sp, #56	; 0x38
00007a8c	e5827000	str	r7, [r2]
00007a90	e59f3160	ldr	r3, [pc, #352]	; 0x7bf8
00007a94	e08f3003	add	r3, pc, r3
00007a98	e5823004	str	r3, [r2, #4]
00007a9c	e582d008	str	sp, [r2, #8]
00007aa0	e28d3018	add	r3, sp, #24	; 0x18
00007aa4	e1a00003	mov	r0, r3
00007aa8	eb000df0	bl	0xb270	; symbol stub for: __Unwind_SjLj_Register
00007aac	e59f3148	ldr	r3, [pc, #328]	; 0x7bfc
00007ab0	e08f3003	add	r3, pc, r3
00007ab4	e2832008	add	r2, r3, #8	; 0x8
00007ab8	e59d304c	ldr	r3, [sp, #76]
00007abc	e5832000	str	r2, [r3]
00007ac0	e59d304c	ldr	r3, [sp, #76]
00007ac4	e5932014	ldr	r2, [r3, #20]
00007ac8	e59f3130	ldr	r3, [pc, #304]	; 0x7c00
00007acc	e08f3003	add	r3, pc, r3
00007ad0	e1520003	cmp	r2, r3
00007ad4	0a00000b	beq	0x7b08
00007ad8	e59d304c	ldr	r3, [sp, #76]
00007adc	e5933014	ldr	r3, [r3, #20]
00007ae0	e58d3004	str	r3, [sp, #4]
00007ae4	e59d2004	ldr	r2, [sp, #4]
00007ae8	e3520000	cmp	r2, #0	; 0x0
00007aec	0a000005	beq	0x7b08
00007af0	e3a03001	mov	r3, #1	; 0x1
00007af4	e58d301c	str	r3, [sp, #28]
00007af8	e59d0004	ldr	r0, [sp, #4]
00007afc	eb000e0e	bl	0xb33c	; symbol stub for: __ZNSsD1Ev
00007b00	e59d0004	ldr	r0, [sp, #4]
00007b04	eb000e0f	bl	0xb348	; symbol stub for: __ZdlPv
00007b08	e59d304c	ldr	r3, [sp, #76]
00007b0c	e2833004	add	r3, r3, #4	; 0x4
00007b10	e58d3008	str	r3, [sp, #8]
00007b14	e3a03002	mov	r3, #2	; 0x2
00007b18	e58d301c	str	r3, [sp, #28]
00007b1c	e59d0008	ldr	r0, [sp, #8]
00007b20	eb000dea	bl	0xb2d0	; symbol stub for: __ZN6google8protobuf15UnknownFieldSetD1Ev
00007b24	e59d304c	ldr	r3, [sp, #76]
00007b28	e58d300c	str	r3, [sp, #12]
00007b2c	e3e03000	mvn	r3, #0	; 0x0
00007b30	e58d301c	str	r3, [sp, #28]
00007b34	e59d000c	ldr	r0, [sp, #12]
00007b38	eb000dea	bl	0xb2e8	; symbol stub for: __ZN6google8protobuf7MessageD2Ev
00007b3c	e3a03000	mov	r3, #0	; 0x0
00007b40	e6ef3073	uxtb r3,r3
00007b44	e3530000	cmp	r3, #0	; 0x0
00007b48	0a000020	beq	0x7bd0
00007b4c	ea00001d	b	0x7bc8
00007b50	e59d301c	ldr	r3, [sp, #28]
00007b54	e59d2020	ldr	r2, [sp, #32]
00007b58	e58d2000	str	r2, [sp]
00007b5c	e3530001	cmp	r3, #1	; 0x1
00007b60	0a00000a	beq	0x7b90
00007b64	e59d3000	ldr	r3, [sp]
00007b68	e58d3010	str	r3, [sp, #16]
00007b6c	e59d304c	ldr	r3, [sp, #76]
00007b70	e2833004	add	r3, r3, #4	; 0x4
00007b74	e58d3008	str	r3, [sp, #8]
00007b78	e3a03000	mov	r3, #0	; 0x0
00007b7c	e58d301c	str	r3, [sp, #28]
00007b80	e59d0008	ldr	r0, [sp, #8]
00007b84	eb000dd1	bl	0xb2d0	; symbol stub for: __ZN6google8protobuf15UnknownFieldSetD1Ev
00007b88	e59d2010	ldr	r2, [sp, #16]
00007b8c	e58d2000	str	r2, [sp]
00007b90	e59d3000	ldr	r3, [sp]
00007b94	e58d3014	str	r3, [sp, #20]
00007b98	e59d204c	ldr	r2, [sp, #76]
00007b9c	e58d200c	str	r2, [sp, #12]
00007ba0	e3a03000	mov	r3, #0	; 0x0
00007ba4	e58d301c	str	r3, [sp, #28]
00007ba8	e59d000c	ldr	r0, [sp, #12]
00007bac	eb000dcd	bl	0xb2e8	; symbol stub for: __ZN6google8protobuf7MessageD2Ev
00007bb0	e59d3014	ldr	r3, [sp, #20]
00007bb4	e58d3000	str	r3, [sp]
00007bb8	e3e03000	mvn	r3, #0	; 0x0
00007bbc	e58d301c	str	r3, [sp, #28]
00007bc0	e59d0000	ldr	r0, [sp]
00007bc4	eb000dac	bl	0xb27c	; symbol stub for: __Unwind_SjLj_Resume
00007bc8	e59d004c	ldr	r0, [sp, #76]
00007bcc	eb000ddd	bl	0xb348	; symbol stub for: __ZdlPv
00007bd0	e28d3018	add	r3, sp, #24	; 0x18
00007bd4	e1a00003	mov	r0, r3
00007bd8	eb000daa	bl	0xb288	; symbol stub for: __Unwind_SjLj_Unregister
00007bdc	e247d05c	sub	sp, r7, #92	; 0x5c
00007be0	ecbd8b11	fldmiax	sp!, {d8-d15}
00007be4	e247d018	sub	sp, r7, #24	; 0x18
00007be8	e8bd0d00	ldmia	sp!, {r8, r10, r11}
00007bec	e8bd80f0	ldmia	sp!, {r4, r5, r6, r7, pc}
00007bf0	000045a8	andeq	r4, r0, r8, lsr #11
00007bf4	00004ba6	andeq	r4, r0, r6, lsr #23
00007bf8	000000b4	streqh	r0, [r0], -r4
00007bfc	00004928	andeq	r4, r0, r8, lsr #18
00007c00	00004c0c	andeq	r4, r0, ip, lsl #24
__ZN3dsp17MicrophoneMessageD1Ev:
00007c04	e92d40f0	stmdb	sp!, {r4, r5, r6, r7, lr}
00007c08	e28d700c	add	r7, sp, #12	; 0xc
00007c0c	e92d0d00	stmdb	sp!, {r8, r10, r11}
00007c10	ed2d8b11	fstmdbx	sp!, {d8-d15}
00007c14	e24dd050	sub	sp, sp, #80	; 0x50
00007c18	e58d004c	str	r0, [sp, #76]
00007c1c	e59f317c	ldr	r3, [pc, #380]	; 0x7da0
00007c20	e08f3003	add	r3, pc, r3
00007c24	e5933000	ldr	r3, [r3]
00007c28	e58d3030	str	r3, [sp, #48]
00007c2c	e59f3170	ldr	r3, [pc, #368]	; 0x7da4
00007c30	e08f3003	add	r3, pc, r3
00007c34	e58d3034	str	r3, [sp, #52]
00007c38	e28d2038	add	r2, sp, #56	; 0x38
00007c3c	e5827000	str	r7, [r2]
00007c40	e59f3160	ldr	r3, [pc, #352]	; 0x7da8
00007c44	e08f3003	add	r3, pc, r3
00007c48	e5823004	str	r3, [r2, #4]
00007c4c	e582d008	str	sp, [r2, #8]
00007c50	e28d3018	add	r3, sp, #24	; 0x18
00007c54	e1a00003	mov	r0, r3
00007c58	eb000d84	bl	0xb270	; symbol stub for: __Unwind_SjLj_Register
00007c5c	e59f3148	ldr	r3, [pc, #328]	; 0x7dac
00007c60	e08f3003	add	r3, pc, r3
00007c64	e2832008	add	r2, r3, #8	; 0x8
00007c68	e59d304c	ldr	r3, [sp, #76]
00007c6c	e5832000	str	r2, [r3]
00007c70	e59d304c	ldr	r3, [sp, #76]
00007c74	e5932014	ldr	r2, [r3, #20]
00007c78	e59f3130	ldr	r3, [pc, #304]	; 0x7db0
00007c7c	e08f3003	add	r3, pc, r3
00007c80	e1520003	cmp	r2, r3
00007c84	0a00000b	beq	0x7cb8
00007c88	e59d304c	ldr	r3, [sp, #76]
00007c8c	e5933014	ldr	r3, [r3, #20]
00007c90	e58d3004	str	r3, [sp, #4]
00007c94	e59d2004	ldr	r2, [sp, #4]
00007c98	e3520000	cmp	r2, #0	; 0x0
00007c9c	0a000005	beq	0x7cb8
00007ca0	e3a03001	mov	r3, #1	; 0x1
00007ca4	e58d301c	str	r3, [sp, #28]
00007ca8	e59d0004	ldr	r0, [sp, #4]
00007cac	eb000da2	bl	0xb33c	; symbol stub for: __ZNSsD1Ev
00007cb0	e59d0004	ldr	r0, [sp, #4]
00007cb4	eb000da3	bl	0xb348	; symbol stub for: __ZdlPv
00007cb8	e59d304c	ldr	r3, [sp, #76]
00007cbc	e2833004	add	r3, r3, #4	; 0x4
00007cc0	e58d3008	str	r3, [sp, #8]
00007cc4	e3a03002	mov	r3, #2	; 0x2
00007cc8	e58d301c	str	r3, [sp, #28]
00007ccc	e59d0008	ldr	r0, [sp, #8]
00007cd0	eb000d7e	bl	0xb2d0	; symbol stub for: __ZN6google8protobuf15UnknownFieldSetD1Ev
00007cd4	e59d304c	ldr	r3, [sp, #76]
00007cd8	e58d300c	str	r3, [sp, #12]
00007cdc	e3e03000	mvn	r3, #0	; 0x0
00007ce0	e58d301c	str	r3, [sp, #28]
00007ce4	e59d000c	ldr	r0, [sp, #12]
00007ce8	eb000d7e	bl	0xb2e8	; symbol stub for: __ZN6google8protobuf7MessageD2Ev
00007cec	e3a03000	mov	r3, #0	; 0x0
00007cf0	e6ef3073	uxtb r3,r3
00007cf4	e3530000	cmp	r3, #0	; 0x0
00007cf8	0a000020	beq	0x7d80
00007cfc	ea00001d	b	0x7d78
00007d00	e59d301c	ldr	r3, [sp, #28]
00007d04	e59d2020	ldr	r2, [sp, #32]
00007d08	e58d2000	str	r2, [sp]
00007d0c	e3530001	cmp	r3, #1	; 0x1
00007d10	0a00000a	beq	0x7d40
00007d14	e59d3000	ldr	r3, [sp]
00007d18	e58d3010	str	r3, [sp, #16]
00007d1c	e59d304c	ldr	r3, [sp, #76]
00007d20	e2833004	add	r3, r3, #4	; 0x4
00007d24	e58d3008	str	r3, [sp, #8]
00007d28	e3a03000	mov	r3, #0	; 0x0
00007d2c	e58d301c	str	r3, [sp, #28]
00007d30	e59d0008	ldr	r0, [sp, #8]
00007d34	eb000d65	bl	0xb2d0	; symbol stub for: __ZN6google8protobuf15UnknownFieldSetD1Ev
00007d38	e59d2010	ldr	r2, [sp, #16]
00007d3c	e58d2000	str	r2, [sp]
00007d40	e59d3000	ldr	r3, [sp]
00007d44	e58d3014	str	r3, [sp, #20]
00007d48	e59d204c	ldr	r2, [sp, #76]
00007d4c	e58d200c	str	r2, [sp, #12]
00007d50	e3a03000	mov	r3, #0	; 0x0
00007d54	e58d301c	str	r3, [sp, #28]
00007d58	e59d000c	ldr	r0, [sp, #12]
00007d5c	eb000d61	bl	0xb2e8	; symbol stub for: __ZN6google8protobuf7MessageD2Ev
00007d60	e59d3014	ldr	r3, [sp, #20]
00007d64	e58d3000	str	r3, [sp]
00007d68	e3e03000	mvn	r3, #0	; 0x0
00007d6c	e58d301c	str	r3, [sp, #28]
00007d70	e59d0000	ldr	r0, [sp]
00007d74	eb000d40	bl	0xb27c	; symbol stub for: __Unwind_SjLj_Resume
00007d78	e59d004c	ldr	r0, [sp, #76]
00007d7c	eb000d71	bl	0xb348	; symbol stub for: __ZdlPv
00007d80	e28d3018	add	r3, sp, #24	; 0x18
00007d84	e1a00003	mov	r0, r3
00007d88	eb000d3e	bl	0xb288	; symbol stub for: __Unwind_SjLj_Unregister
00007d8c	e247d05c	sub	sp, r7, #92	; 0x5c
00007d90	ecbd8b11	fldmiax	sp!, {d8-d15}
00007d94	e247d018	sub	sp, r7, #24	; 0x18
00007d98	e8bd0d00	ldmia	sp!, {r8, r10, r11}
00007d9c	e8bd80f0	ldmia	sp!, {r4, r5, r6, r7, pc}
00007da0	000043f8	streqd	r4, [r0], -r8
00007da4	000049fe	streqd	r4, [r0], -lr
00007da8	000000b4	streqh	r0, [r0], -r4
00007dac	00004778	andeq	r4, r0, r8, ror r7
00007db0	00004a5c	andeq	r4, r0, ip, asr r10
__ZN3dsp17MicrophoneMessageD0Ev:
00007db4	e92d40f0	stmdb	sp!, {r4, r5, r6, r7, lr}
00007db8	e28d700c	add	r7, sp, #12	; 0xc
00007dbc	e92d0d00	stmdb	sp!, {r8, r10, r11}
00007dc0	ed2d8b11	fstmdbx	sp!, {d8-d15}
00007dc4	e24dd050	sub	sp, sp, #80	; 0x50
00007dc8	e58d004c	str	r0, [sp, #76]
00007dcc	e59f317c	ldr	r3, [pc, #380]	; 0x7f50
00007dd0	e08f3003	add	r3, pc, r3
00007dd4	e5933000	ldr	r3, [r3]
00007dd8	e58d3030	str	r3, [sp, #48]
00007ddc	e59f3170	ldr	r3, [pc, #368]	; 0x7f54
00007de0	e08f3003	add	r3, pc, r3
00007de4	e58d3034	str	r3, [sp, #52]
00007de8	e28d2038	add	r2, sp, #56	; 0x38
00007dec	e5827000	str	r7, [r2]
00007df0	e59f3160	ldr	r3, [pc, #352]	; 0x7f58
00007df4	e08f3003	add	r3, pc, r3
00007df8	e5823004	str	r3, [r2, #4]
00007dfc	e582d008	str	sp, [r2, #8]
00007e00	e28d3018	add	r3, sp, #24	; 0x18
00007e04	e1a00003	mov	r0, r3
00007e08	eb000d18	bl	0xb270	; symbol stub for: __Unwind_SjLj_Register
00007e0c	e59f3148	ldr	r3, [pc, #328]	; 0x7f5c
00007e10	e08f3003	add	r3, pc, r3
00007e14	e2832008	add	r2, r3, #8	; 0x8
00007e18	e59d304c	ldr	r3, [sp, #76]
00007e1c	e5832000	str	r2, [r3]
00007e20	e59d304c	ldr	r3, [sp, #76]
00007e24	e5932014	ldr	r2, [r3, #20]
00007e28	e59f3130	ldr	r3, [pc, #304]	; 0x7f60
00007e2c	e08f3003	add	r3, pc, r3
00007e30	e1520003	cmp	r2, r3
00007e34	0a00000b	beq	0x7e68
00007e38	e59d304c	ldr	r3, [sp, #76]
00007e3c	e5933014	ldr	r3, [r3, #20]
00007e40	e58d3004	str	r3, [sp, #4]
00007e44	e59d2004	ldr	r2, [sp, #4]
00007e48	e3520000	cmp	r2, #0	; 0x0
00007e4c	0a000005	beq	0x7e68
00007e50	e3a03001	mov	r3, #1	; 0x1
00007e54	e58d301c	str	r3, [sp, #28]
00007e58	e59d0004	ldr	r0, [sp, #4]
00007e5c	eb000d36	bl	0xb33c	; symbol stub for: __ZNSsD1Ev
00007e60	e59d0004	ldr	r0, [sp, #4]
00007e64	eb000d37	bl	0xb348	; symbol stub for: __ZdlPv
00007e68	e59d304c	ldr	r3, [sp, #76]
00007e6c	e2833004	add	r3, r3, #4	; 0x4
00007e70	e58d3008	str	r3, [sp, #8]
00007e74	e3a03002	mov	r3, #2	; 0x2
00007e78	e58d301c	str	r3, [sp, #28]
00007e7c	e59d0008	ldr	r0, [sp, #8]
00007e80	eb000d12	bl	0xb2d0	; symbol stub for: __ZN6google8protobuf15UnknownFieldSetD1Ev
00007e84	e59d304c	ldr	r3, [sp, #76]
00007e88	e58d300c	str	r3, [sp, #12]
00007e8c	e3e03000	mvn	r3, #0	; 0x0
00007e90	e58d301c	str	r3, [sp, #28]
00007e94	e59d000c	ldr	r0, [sp, #12]
00007e98	eb000d12	bl	0xb2e8	; symbol stub for: __ZN6google8protobuf7MessageD2Ev
00007e9c	e3a03001	mov	r3, #1	; 0x1
00007ea0	e6ef3073	uxtb r3,r3
00007ea4	e3530000	cmp	r3, #0	; 0x0
00007ea8	0a000020	beq	0x7f30
00007eac	ea00001d	b	0x7f28
00007eb0	e59d301c	ldr	r3, [sp, #28]
00007eb4	e59d2020	ldr	r2, [sp, #32]
00007eb8	e58d2000	str	r2, [sp]
00007ebc	e3530001	cmp	r3, #1	; 0x1
00007ec0	0a00000a	beq	0x7ef0
00007ec4	e59d3000	ldr	r3, [sp]
00007ec8	e58d3010	str	r3, [sp, #16]
00007ecc	e59d304c	ldr	r3, [sp, #76]
00007ed0	e2833004	add	r3, r3, #4	; 0x4
00007ed4	e58d3008	str	r3, [sp, #8]
00007ed8	e3a03000	mov	r3, #0	; 0x0
00007edc	e58d301c	str	r3, [sp, #28]
00007ee0	e59d0008	ldr	r0, [sp, #8]
00007ee4	eb000cf9	bl	0xb2d0	; symbol stub for: __ZN6google8protobuf15UnknownFieldSetD1Ev
00007ee8	e59d2010	ldr	r2, [sp, #16]
00007eec	e58d2000	str	r2, [sp]
00007ef0	e59d3000	ldr	r3, [sp]
00007ef4	e58d3014	str	r3, [sp, #20]
00007ef8	e59d204c	ldr	r2, [sp, #76]
00007efc	e58d200c	str	r2, [sp, #12]
00007f00	e3a03000	mov	r3, #0	; 0x0
00007f04	e58d301c	str	r3, [sp, #28]
00007f08	e59d000c	ldr	r0, [sp, #12]
00007f0c	eb000cf5	bl	0xb2e8	; symbol stub for: __ZN6google8protobuf7MessageD2Ev
00007f10	e59d3014	ldr	r3, [sp, #20]
00007f14	e58d3000	str	r3, [sp]
00007f18	e3e03000	mvn	r3, #0	; 0x0
00007f1c	e58d301c	str	r3, [sp, #28]
00007f20	e59d0000	ldr	r0, [sp]
00007f24	eb000cd4	bl	0xb27c	; symbol stub for: __Unwind_SjLj_Resume
00007f28	e59d004c	ldr	r0, [sp, #76]
00007f2c	eb000d05	bl	0xb348	; symbol stub for: __ZdlPv
00007f30	e28d3018	add	r3, sp, #24	; 0x18
00007f34	e1a00003	mov	r0, r3
00007f38	eb000cd2	bl	0xb288	; symbol stub for: __Unwind_SjLj_Unregister
00007f3c	e247d05c	sub	sp, r7, #92	; 0x5c
00007f40	ecbd8b11	fldmiax	sp!, {d8-d15}
00007f44	e247d018	sub	sp, r7, #24	; 0x18
00007f48	e8bd0d00	ldmia	sp!, {r8, r10, r11}
00007f4c	e8bd80f0	ldmia	sp!, {r4, r5, r6, r7, pc}
00007f50	00004248	andeq	r4, r0, r8, asr #4
00007f54	00004856	andeq	r4, r0, r6, asr r8
00007f58	000000b4	streqh	r0, [r0], -r4
00007f5c	000045c8	andeq	r4, r0, r8, asr #11
00007f60	000048ac	andeq	r4, r0, ip, lsr #17
__ZN3dsp17MicrophoneMessage10descriptorEv:
00007f64	e92d4080	stmdb	sp!, {r7, lr}
00007f68	e28d7000	add	r7, sp, #0	; 0x0
00007f6c	e59f3024	ldr	r3, [pc, #36]	; 0x7f98
00007f70	e08f3003	add	r3, pc, r3
00007f74	e5933000	ldr	r3, [r3]
00007f78	e3530000	cmp	r3, #0	; 0x0
00007f7c	1a000000	bne	0x7f84
00007f80	ebffed1a	bl	__ZN3dsp30protobuf_BuildDesc_dsp_2eprotoEv
00007f84	e59f3010	ldr	r3, [pc, #16]	; 0x7f9c
00007f88	e08f3003	add	r3, pc, r3
00007f8c	e5933000	ldr	r3, [r3]
00007f90	e1a00003	mov	r0, r3
00007f94	e8bd8080	ldmia	sp!, {r7, pc}
00007f98	00004188	andeq	r4, r0, r8, lsl #3
00007f9c	00004170	andeq	r4, r0, r0, ror r1
__ZN3dsp17MicrophoneMessage16default_instanceEv:
00007fa0	e92d4080	stmdb	sp!, {r7, lr}
00007fa4	e28d7000	add	r7, sp, #0	; 0x0
00007fa8	e59f3024	ldr	r3, [pc, #36]	; 0x7fd4
00007fac	e08f3003	add	r3, pc, r3
00007fb0	e5933000	ldr	r3, [r3]
00007fb4	e3530000	cmp	r3, #0	; 0x0
00007fb8	1a000000	bne	0x7fc0
00007fbc	ebffed0b	bl	__ZN3dsp30protobuf_BuildDesc_dsp_2eprotoEv
00007fc0	e59f3010	ldr	r3, [pc, #16]	; 0x7fd8
00007fc4	e08f3003	add	r3, pc, r3
00007fc8	e5933000	ldr	r3, [r3]
00007fcc	e1a00003	mov	r0, r3
00007fd0	e8bd8080	ldmia	sp!, {r7, pc}
00007fd4	00004120	andeq	r4, r0, r0, lsr #2
00007fd8	00004108	andeq	r4, r0, r8, lsl #2
__ZNK3dsp17MicrophoneMessage3NewEv:
00007fdc	e92d40f0	stmdb	sp!, {r4, r5, r6, r7, lr}
00007fe0	e28d700c	add	r7, sp, #12	; 0xc
00007fe4	e92d0d00	stmdb	sp!, {r8, r10, r11}
00007fe8	ed2d8b11	fstmdbx	sp!, {d8-d15}
00007fec	e24dd048	sub	sp, sp, #72	; 0x48
00007ff0	e58d0044	str	r0, [sp, #68]
00007ff4	e59f30c0	ldr	r3, [pc, #192]	; 0x80bc
00007ff8	e08f3003	add	r3, pc, r3
00007ffc	e5933000	ldr	r3, [r3]
00008000	e58d3028	str	r3, [sp, #40]
00008004	e59f30b4	ldr	r3, [pc, #180]	; 0x80c0
00008008	e08f3003	add	r3, pc, r3
0000800c	e58d302c	str	r3, [sp, #44]
00008010	e28d2030	add	r2, sp, #48	; 0x30
00008014	e5827000	str	r7, [r2]
00008018	e59f30a4	ldr	r3, [pc, #164]	; 0x80c4
0000801c	e08f3003	add	r3, pc, r3
00008020	e5823004	str	r3, [r2, #4]
00008024	e582d008	str	sp, [r2, #8]
00008028	e28d3010	add	r3, sp, #16	; 0x10
0000802c	e1a00003	mov	r0, r3
00008030	eb000c8e	bl	0xb270	; symbol stub for: __Unwind_SjLj_Register
00008034	e3e03000	mvn	r3, #0	; 0x0
00008038	e58d3014	str	r3, [sp, #20]
0000803c	e3a0001c	mov	r0, #28	; 0x1c
00008040	eb000cc3	bl	0xb354	; symbol stub for: __Znwm
00008044	e1a03000	mov	r3, r0
00008048	e58d3008	str	r3, [sp, #8]
0000804c	e3a03001	mov	r3, #1	; 0x1
00008050	e58d3014	str	r3, [sp, #20]
00008054	e59d0008	ldr	r0, [sp, #8]
00008058	ebffede4	bl	__ZN3dsp17MicrophoneMessageC1Ev
0000805c	ea00000b	b	0x8090
00008060	e59d3018	ldr	r3, [sp, #24]
00008064	e58d3000	str	r3, [sp]
00008068	e59d3000	ldr	r3, [sp]
0000806c	e58d300c	str	r3, [sp, #12]
00008070	e59d0008	ldr	r0, [sp, #8]
00008074	eb000cb3	bl	0xb348	; symbol stub for: __ZdlPv
00008078	e59d300c	ldr	r3, [sp, #12]
0000807c	e58d3000	str	r3, [sp]
00008080	e3e03000	mvn	r3, #0	; 0x0
00008084	e58d3014	str	r3, [sp, #20]
00008088	e59d0000	ldr	r0, [sp]
0000808c	eb000c7a	bl	0xb27c	; symbol stub for: __Unwind_SjLj_Resume
00008090	e59d3008	ldr	r3, [sp, #8]
00008094	e58d3004	str	r3, [sp, #4]
00008098	e28d3010	add	r3, sp, #16	; 0x10
0000809c	e1a00003	mov	r0, r3
000080a0	eb000c78	bl	0xb288	; symbol stub for: __Unwind_SjLj_Unregister
000080a4	e59d0004	ldr	r0, [sp, #4]
000080a8	e247d05c	sub	sp, r7, #92	; 0x5c
000080ac	ecbd8b11	fldmiax	sp!, {d8-d15}
000080b0	e247d018	sub	sp, r7, #24	; 0x18
000080b4	e8bd0d00	ldmia	sp!, {r8, r10, r11}
000080b8	e8bd80f0	ldmia	sp!, {r4, r5, r6, r7, pc}
000080bc	00004020	andeq	r4, r0, r0, lsr #32
000080c0	00004636	andeq	r4, r0, r6, lsr r6
000080c4	0000003c	andeq	r0, r0, ip, lsr r0
__ZNK3dsp17MicrophoneMessage13GetDescriptorEv:
000080c8	e92d4080	stmdb	sp!, {r7, lr}
000080cc	e28d7000	add	r7, sp, #0	; 0x0
000080d0	e24dd004	sub	sp, sp, #4	; 0x4
000080d4	e58d0000	str	r0, [sp]
000080d8	ebffffa1	bl	__ZN3dsp17MicrophoneMessage10descriptorEv
000080dc	e1a03000	mov	r3, r0
000080e0	e1a00003	mov	r0, r3
000080e4	e247d000	sub	sp, r7, #0	; 0x0
000080e8	e8bd8080	ldmia	sp!, {r7, pc}
__ZNK3dsp17MicrophoneMessage13GetReflectionEv:
000080ec	e92d4080	stmdb	sp!, {r7, lr}
000080f0	e28d7000	add	r7, sp, #0	; 0x0
000080f4	e24dd004	sub	sp, sp, #4	; 0x4
000080f8	e58d0000	str	r0, [sp]
000080fc	e59f3028	ldr	r3, [pc, #40]	; 0x812c
00008100	e08f3003	add	r3, pc, r3
00008104	e5933000	ldr	r3, [r3]
00008108	e3530000	cmp	r3, #0	; 0x0
0000810c	1a000000	bne	0x8114
00008110	ebffecb6	bl	__ZN3dsp30protobuf_BuildDesc_dsp_2eprotoEv
00008114	e59f3014	ldr	r3, [pc, #20]	; 0x8130
00008118	e08f3003	add	r3, pc, r3
0000811c	e5933000	ldr	r3, [r3]
00008120	e1a00003	mov	r0, r3
00008124	e247d000	sub	sp, r7, #0	; 0x0
00008128	e8bd8080	ldmia	sp!, {r7, pc}
0000812c	00003ff4	streqd	r3, [r0], -r4
00008130	00003fdc	ldreqd	r3, [r0], -ip
__ZN3dsp19LocationDescriptionC2Ev:
00008134	e92d40f0	stmdb	sp!, {r4, r5, r6, r7, lr}
00008138	e28d700c	add	r7, sp, #12	; 0xc
0000813c	e92d0d00	stmdb	sp!, {r8, r10, r11}
00008140	ed2d8b11	fstmdbx	sp!, {d8-d15}
00008144	e24dd040	sub	sp, sp, #64	; 0x40
00008148	e58d003c	str	r0, [sp, #60]
0000814c	e59f3104	ldr	r3, [pc, #260]	; 0x8258
00008150	e08f3003	add	r3, pc, r3
00008154	e5933000	ldr	r3, [r3]
00008158	e58d3020	str	r3, [sp, #32]
0000815c	e59f30f8	ldr	r3, [pc, #248]	; 0x825c
00008160	e08f3003	add	r3, pc, r3
00008164	e58d3024	str	r3, [sp, #36]
00008168	e28d2028	add	r2, sp, #40	; 0x28
0000816c	e5827000	str	r7, [r2]
00008170	e59f30e8	ldr	r3, [pc, #232]	; 0x8260
00008174	e08f3003	add	r3, pc, r3
00008178	e5823004	str	r3, [r2, #4]
0000817c	e582d008	str	sp, [r2, #8]
00008180	e28d3008	add	r3, sp, #8	; 0x8
00008184	e1a00003	mov	r0, r3
00008188	eb000c38	bl	0xb270	; symbol stub for: __Unwind_SjLj_Register
0000818c	e59d303c	ldr	r3, [sp, #60]
00008190	e1a00003	mov	r0, r3
00008194	e59f30c8	ldr	r3, [pc, #200]	; 0x8264
00008198	e08f3003	add	r3, pc, r3
0000819c	e5933000	ldr	r3, [r3]
000081a0	e12fff33	blx	r3
000081a4	e59f30bc	ldr	r3, [pc, #188]	; 0x8268
000081a8	e08f3003	add	r3, pc, r3
000081ac	e2832008	add	r2, r3, #8	; 0x8
000081b0	e59d303c	ldr	r3, [sp, #60]
000081b4	e5832000	str	r2, [r3]
000081b8	e59d303c	ldr	r3, [sp, #60]
000081bc	e2832004	add	r2, r3, #4	; 0x4
000081c0	e3a03001	mov	r3, #1	; 0x1
000081c4	e58d300c	str	r3, [sp, #12]
000081c8	e1a00002	mov	r0, r2
000081cc	eb000c3c	bl	0xb2c4	; symbol stub for: __ZN6google8protobuf15UnknownFieldSetC1Ev
000081d0	e59d203c	ldr	r2, [sp, #60]
000081d4	e3a03000	mov	r3, #0	; 0x0
000081d8	e5823008	str	r3, [r2, #8]
000081dc	e59d203c	ldr	r2, [sp, #60]
000081e0	e59f3084	ldr	r3, [pc, #132]	; 0x826c
000081e4	e582300c	str	r3, [r2, #12]
000081e8	e59d303c	ldr	r3, [sp, #60]
000081ec	e2832010	add	r2, r3, #16	; 0x10
000081f0	e3a03000	mov	r3, #0	; 0x0
000081f4	e5823000	str	r3, [r2]
000081f8	ea00000e	b	0x8238
000081fc	e59d3010	ldr	r3, [sp, #16]
00008200	e58d3000	str	r3, [sp]
00008204	e59d3000	ldr	r3, [sp]
00008208	e58d3004	str	r3, [sp, #4]
0000820c	e59d203c	ldr	r2, [sp, #60]
00008210	e3a03000	mov	r3, #0	; 0x0
00008214	e58d300c	str	r3, [sp, #12]
00008218	e1a00002	mov	r0, r2
0000821c	eb000c31	bl	0xb2e8	; symbol stub for: __ZN6google8protobuf7MessageD2Ev
00008220	e59d3004	ldr	r3, [sp, #4]
00008224	e58d3000	str	r3, [sp]
00008228	e3e03000	mvn	r3, #0	; 0x0
0000822c	e58d300c	str	r3, [sp, #12]
00008230	e59d0000	ldr	r0, [sp]
00008234	eb000c10	bl	0xb27c	; symbol stub for: __Unwind_SjLj_Resume
00008238	e28d3008	add	r3, sp, #8	; 0x8
0000823c	e1a00003	mov	r0, r3
00008240	eb000c10	bl	0xb288	; symbol stub for: __Unwind_SjLj_Unregister
00008244	e247d05c	sub	sp, r7, #92	; 0x5c
00008248	ecbd8b11	fldmiax	sp!, {d8-d15}
0000824c	e247d018	sub	sp, r7, #24	; 0x18
00008250	e8bd0d00	ldmia	sp!, {r8, r10, r11}
00008254	e8bd80f0	ldmia	sp!, {r4, r5, r6, r7, pc}
00008258	00003ec8	andeq	r3, r0, r8, asr #29
0000825c	000044e4	andeq	r4, r0, r4, ror #9
00008260	00000080	andeq	r0, r0, r0, lsl #1
00008264	00003e88	andeq	r3, r0, r8, lsl #29
00008268	00004278	andeq	r4, r0, r8, ror r2
0000826c	00000000	andeq	r0, r0, r0
__ZN3dsp19LocationDescriptionC2ERKS0_:
00008270	e92d40f0	stmdb	sp!, {r4, r5, r6, r7, lr}
00008274	e28d700c	add	r7, sp, #12	; 0xc
00008278	e92d0d00	stmdb	sp!, {r8, r10, r11}
0000827c	ed2d8b11	fstmdbx	sp!, {d8-d15}
00008280	e24dd048	sub	sp, sp, #72	; 0x48
00008284	e58d0044	str	r0, [sp, #68]
00008288	e58d1040	str	r1, [sp, #64]
0000828c	e59f3150	ldr	r3, [pc, #336]	; 0x83e4
00008290	e08f3003	add	r3, pc, r3
00008294	e5933000	ldr	r3, [r3]
00008298	e58d3024	str	r3, [sp, #36]
0000829c	e59f3144	ldr	r3, [pc, #324]	; 0x83e8
000082a0	e08f3003	add	r3, pc, r3
000082a4	e58d3028	str	r3, [sp, #40]
000082a8	e28d202c	add	r2, sp, #44	; 0x2c
000082ac	e5827000	str	r7, [r2]
000082b0	e59f3134	ldr	r3, [pc, #308]	; 0x83ec
000082b4	e08f3003	add	r3, pc, r3
000082b8	e5823004	str	r3, [r2, #4]
000082bc	e582d008	str	sp, [r2, #8]
000082c0	e28d300c	add	r3, sp, #12	; 0xc
000082c4	e1a00003	mov	r0, r3
000082c8	eb000be8	bl	0xb270	; symbol stub for: __Unwind_SjLj_Register
000082cc	e59d3044	ldr	r3, [sp, #68]
000082d0	e1a00003	mov	r0, r3
000082d4	e59f3114	ldr	r3, [pc, #276]	; 0x83f0
000082d8	e08f3003	add	r3, pc, r3
000082dc	e5933000	ldr	r3, [r3]
000082e0	e12fff33	blx	r3
000082e4	e59f3108	ldr	r3, [pc, #264]	; 0x83f4
000082e8	e08f3003	add	r3, pc, r3
000082ec	e2832008	add	r2, r3, #8	; 0x8
000082f0	e59d3044	ldr	r3, [sp, #68]
000082f4	e5832000	str	r2, [r3]
000082f8	e59d3044	ldr	r3, [sp, #68]
000082fc	e2832004	add	r2, r3, #4	; 0x4
00008300	e3a03002	mov	r3, #2	; 0x2
00008304	e58d3010	str	r3, [sp, #16]
00008308	e1a00002	mov	r0, r2
0000830c	eb000bec	bl	0xb2c4	; symbol stub for: __ZN6google8protobuf15UnknownFieldSetC1Ev
00008310	e59d2044	ldr	r2, [sp, #68]
00008314	e3a03000	mov	r3, #0	; 0x0
00008318	e5823008	str	r3, [r2, #8]
0000831c	e59d2044	ldr	r2, [sp, #68]
00008320	e59f30d0	ldr	r3, [pc, #208]	; 0x83f8
00008324	e582300c	str	r3, [r2, #12]
00008328	e59d3044	ldr	r3, [sp, #68]
0000832c	e2832010	add	r2, r3, #16	; 0x10
00008330	e3a03000	mov	r3, #0	; 0x0
00008334	e5823000	str	r3, [r2]
00008338	e59d2044	ldr	r2, [sp, #68]
0000833c	e59d1040	ldr	r1, [sp, #64]
00008340	e3a03001	mov	r3, #1	; 0x1
00008344	e58d3010	str	r3, [sp, #16]
00008348	e1a00002	mov	r0, r2
0000834c	eb000be2	bl	0xb2dc	; symbol stub for: __ZN6google8protobuf7Message9MergeFromERKS1_
00008350	ea00001b	b	0x83c4
00008354	e59d3010	ldr	r3, [sp, #16]
00008358	e59d2014	ldr	r2, [sp, #20]
0000835c	e58d2000	str	r2, [sp]
00008360	e3530001	cmp	r3, #1	; 0x1
00008364	0a000009	beq	0x8390
00008368	e59d3000	ldr	r3, [sp]
0000836c	e58d3004	str	r3, [sp, #4]
00008370	e59d3044	ldr	r3, [sp, #68]
00008374	e2832004	add	r2, r3, #4	; 0x4
00008378	e3a03000	mov	r3, #0	; 0x0
0000837c	e58d3010	str	r3, [sp, #16]
00008380	e1a00002	mov	r0, r2
00008384	eb000bd1	bl	0xb2d0	; symbol stub for: __ZN6google8protobuf15UnknownFieldSetD1Ev
00008388	e59d2004	ldr	r2, [sp, #4]
0000838c	e58d2000	str	r2, [sp]
00008390	e59d3000	ldr	r3, [sp]
00008394	e58d3008	str	r3, [sp, #8]
00008398	e59d2044	ldr	r2, [sp, #68]
0000839c	e3a03000	mov	r3, #0	; 0x0
000083a0	e58d3010	str	r3, [sp, #16]
000083a4	e1a00002	mov	r0, r2
000083a8	eb000bce	bl	0xb2e8	; symbol stub for: __ZN6google8protobuf7MessageD2Ev
000083ac	e59d2008	ldr	r2, [sp, #8]
000083b0	e58d2000	str	r2, [sp]
000083b4	e3e03000	mvn	r3, #0	; 0x0
000083b8	e58d3010	str	r3, [sp, #16]
000083bc	e59d0000	ldr	r0, [sp]
000083c0	eb000bad	bl	0xb27c	; symbol stub for: __Unwind_SjLj_Resume
000083c4	e28d300c	add	r3, sp, #12	; 0xc
000083c8	e1a00003	mov	r0, r3
000083cc	eb000bad	bl	0xb288	; symbol stub for: __Unwind_SjLj_Unregister
000083d0	e247d05c	sub	sp, r7, #92	; 0x5c
000083d4	ecbd8b11	fldmiax	sp!, {d8-d15}
000083d8	e247d018	sub	sp, r7, #24	; 0x18
000083dc	e8bd0d00	ldmia	sp!, {r8, r10, r11}
000083e0	e8bd80f0	ldmia	sp!, {r4, r5, r6, r7, pc}
000083e4	00003d88	andeq	r3, r0, r8, lsl #27
000083e8	000043aa	andeq	r4, r0, r10, lsr #7
000083ec	00000098	muleq	r0, r8, r0
000083f0	00003d48	andeq	r3, r0, r8, asr #26
000083f4	00004138	andeq	r4, r0, r8, lsr r1
000083f8	00000000	andeq	r0, r0, r0
__ZN3dsp19LocationDescriptionC1ERKS0_:
000083fc	e92d40f0	stmdb	sp!, {r4, r5, r6, r7, lr}
00008400	e28d700c	add	r7, sp, #12	; 0xc
00008404	e92d0d00	stmdb	sp!, {r8, r10, r11}
00008408	ed2d8b11	fstmdbx	sp!, {d8-d15}
0000840c	e24dd048	sub	sp, sp, #72	; 0x48
00008410	e58d0044	str	r0, [sp, #68]
00008414	e58d1040	str	r1, [sp, #64]
00008418	e59f3150	ldr	r3, [pc, #336]	; 0x8570
0000841c	e08f3003	add	r3, pc, r3
00008420	e5933000	ldr	r3, [r3]
00008424	e58d3024	str	r3, [sp, #36]
00008428	e59f3144	ldr	r3, [pc, #324]	; 0x8574
0000842c	e08f3003	add	r3, pc, r3
00008430	e58d3028	str	r3, [sp, #40]
00008434	e28d202c	add	r2, sp, #44	; 0x2c
00008438	e5827000	str	r7, [r2]
0000843c	e59f3134	ldr	r3, [pc, #308]	; 0x8578
00008440	e08f3003	add	r3, pc, r3
00008444	e5823004	str	r3, [r2, #4]
00008448	e582d008	str	sp, [r2, #8]
0000844c	e28d300c	add	r3, sp, #12	; 0xc
00008450	e1a00003	mov	r0, r3
00008454	eb000b85	bl	0xb270	; symbol stub for: __Unwind_SjLj_Register
00008458	e59d3044	ldr	r3, [sp, #68]
0000845c	e1a00003	mov	r0, r3
00008460	e59f3114	ldr	r3, [pc, #276]	; 0x857c
00008464	e08f3003	add	r3, pc, r3
00008468	e5933000	ldr	r3, [r3]
0000846c	e12fff33	blx	r3
00008470	e59f3108	ldr	r3, [pc, #264]	; 0x8580
00008474	e08f3003	add	r3, pc, r3
00008478	e2832008	add	r2, r3, #8	; 0x8
0000847c	e59d3044	ldr	r3, [sp, #68]
00008480	e5832000	str	r2, [r3]
00008484	e59d3044	ldr	r3, [sp, #68]
00008488	e2832004	add	r2, r3, #4	; 0x4
0000848c	e3a03002	mov	r3, #2	; 0x2
00008490	e58d3010	str	r3, [sp, #16]
00008494	e1a00002	mov	r0, r2
00008498	eb000b89	bl	0xb2c4	; symbol stub for: __ZN6google8protobuf15UnknownFieldSetC1Ev
0000849c	e59d2044	ldr	r2, [sp, #68]
000084a0	e3a03000	mov	r3, #0	; 0x0
000084a4	e5823008	str	r3, [r2, #8]
000084a8	e59d2044	ldr	r2, [sp, #68]
000084ac	e59f30d0	ldr	r3, [pc, #208]	; 0x8584
000084b0	e582300c	str	r3, [r2, #12]
000084b4	e59d3044	ldr	r3, [sp, #68]
000084b8	e2832010	add	r2, r3, #16	; 0x10
000084bc	e3a03000	mov	r3, #0	; 0x0
000084c0	e5823000	str	r3, [r2]
000084c4	e59d2044	ldr	r2, [sp, #68]
000084c8	e59d1040	ldr	r1, [sp, #64]
000084cc	e3a03001	mov	r3, #1	; 0x1
000084d0	e58d3010	str	r3, [sp, #16]
000084d4	e1a00002	mov	r0, r2
000084d8	eb000b7f	bl	0xb2dc	; symbol stub for: __ZN6google8protobuf7Message9MergeFromERKS1_
000084dc	ea00001b	b	0x8550
000084e0	e59d3010	ldr	r3, [sp, #16]
000084e4	e59d2014	ldr	r2, [sp, #20]
000084e8	e58d2000	str	r2, [sp]
000084ec	e3530001	cmp	r3, #1	; 0x1
000084f0	0a000009	beq	0x851c
000084f4	e59d3000	ldr	r3, [sp]
000084f8	e58d3004	str	r3, [sp, #4]
000084fc	e59d3044	ldr	r3, [sp, #68]
00008500	e2832004	add	r2, r3, #4	; 0x4
00008504	e3a03000	mov	r3, #0	; 0x0
00008508	e58d3010	str	r3, [sp, #16]
0000850c	e1a00002	mov	r0, r2
00008510	eb000b6e	bl	0xb2d0	; symbol stub for: __ZN6google8protobuf15UnknownFieldSetD1Ev
00008514	e59d2004	ldr	r2, [sp, #4]
00008518	e58d2000	str	r2, [sp]
0000851c	e59d3000	ldr	r3, [sp]
00008520	e58d3008	str	r3, [sp, #8]
00008524	e59d2044	ldr	r2, [sp, #68]
00008528	e3a03000	mov	r3, #0	; 0x0
0000852c	e58d3010	str	r3, [sp, #16]
00008530	e1a00002	mov	r0, r2
00008534	eb000b6b	bl	0xb2e8	; symbol stub for: __ZN6google8protobuf7MessageD2Ev
00008538	e59d2008	ldr	r2, [sp, #8]
0000853c	e58d2000	str	r2, [sp]
00008540	e3e03000	mvn	r3, #0	; 0x0
00008544	e58d3010	str	r3, [sp, #16]
00008548	e59d0000	ldr	r0, [sp]
0000854c	eb000b4a	bl	0xb27c	; symbol stub for: __Unwind_SjLj_Resume
00008550	e28d300c	add	r3, sp, #12	; 0xc
00008554	e1a00003	mov	r0, r3
00008558	eb000b4a	bl	0xb288	; symbol stub for: __Unwind_SjLj_Unregister
0000855c	e247d05c	sub	sp, r7, #92	; 0x5c
00008560	ecbd8b11	fldmiax	sp!, {d8-d15}
00008564	e247d018	sub	sp, r7, #24	; 0x18
00008568	e8bd0d00	ldmia	sp!, {r8, r10, r11}
0000856c	e8bd80f0	ldmia	sp!, {r4, r5, r6, r7, pc}
00008570	00003bfc	streqd	r3, [r0], -ip
00008574	00004226	andeq	r4, r0, r6, lsr #4
00008578	00000098	muleq	r0, r8, r0
0000857c	00003bbc	streqh	r3, [r0], -ip
00008580	00003fac	andeq	r3, r0, ip, lsr #31
00008584	00000000	andeq	r0, r0, r0
__ZN3dsp19LocationDescriptionD2Ev:
00008588	e92d40f0	stmdb	sp!, {r4, r5, r6, r7, lr}
0000858c	e28d700c	add	r7, sp, #12	; 0xc
00008590	e92d0d00	stmdb	sp!, {r8, r10, r11}
00008594	ed2d8b11	fstmdbx	sp!, {d8-d15}
00008598	e24dd044	sub	sp, sp, #68	; 0x44
0000859c	e58d0040	str	r0, [sp, #64]
000085a0	e59f30f8	ldr	r3, [pc, #248]	; 0x86a0
000085a4	e08f3003	add	r3, pc, r3
000085a8	e5933000	ldr	r3, [r3]
000085ac	e58d3024	str	r3, [sp, #36]
000085b0	e59f30ec	ldr	r3, [pc, #236]	; 0x86a4
000085b4	e08f3003	add	r3, pc, r3
000085b8	e58d3028	str	r3, [sp, #40]
000085bc	e28d202c	add	r2, sp, #44	; 0x2c
000085c0	e5827000	str	r7, [r2]
000085c4	e59f30dc	ldr	r3, [pc, #220]	; 0x86a8
000085c8	e08f3003	add	r3, pc, r3
000085cc	e5823004	str	r3, [r2, #4]
000085d0	e582d008	str	sp, [r2, #8]
000085d4	e28d300c	add	r3, sp, #12	; 0xc
000085d8	e1a00003	mov	r0, r3
000085dc	eb000b23	bl	0xb270	; symbol stub for: __Unwind_SjLj_Register
000085e0	e59f30c4	ldr	r3, [pc, #196]	; 0x86ac
000085e4	e08f3003	add	r3, pc, r3
000085e8	e2832008	add	r2, r3, #8	; 0x8
000085ec	e59d3040	ldr	r3, [sp, #64]
000085f0	e5832000	str	r2, [r3]
000085f4	e59d3040	ldr	r3, [sp, #64]
000085f8	e2832004	add	r2, r3, #4	; 0x4
000085fc	e3a03001	mov	r3, #1	; 0x1
00008600	e58d3010	str	r3, [sp, #16]
00008604	e1a00002	mov	r0, r2
00008608	eb000b30	bl	0xb2d0	; symbol stub for: __ZN6google8protobuf15UnknownFieldSetD1Ev
0000860c	e59d3040	ldr	r3, [sp, #64]
00008610	e58d3004	str	r3, [sp, #4]
00008614	e3e03000	mvn	r3, #0	; 0x0
00008618	e58d3010	str	r3, [sp, #16]
0000861c	e59d0004	ldr	r0, [sp, #4]
00008620	eb000b30	bl	0xb2e8	; symbol stub for: __ZN6google8protobuf7MessageD2Ev
00008624	e3a03000	mov	r3, #0	; 0x0
00008628	e6ef3073	uxtb r3,r3
0000862c	e3530000	cmp	r3, #0	; 0x0
00008630	0a000012	beq	0x8680
00008634	ea00000f	b	0x8678
00008638	e59d3014	ldr	r3, [sp, #20]
0000863c	e58d3000	str	r3, [sp]
00008640	e59d3000	ldr	r3, [sp]
00008644	e58d3008	str	r3, [sp, #8]
00008648	e59d3040	ldr	r3, [sp, #64]
0000864c	e58d3004	str	r3, [sp, #4]
00008650	e3a03000	mov	r3, #0	; 0x0
00008654	e58d3010	str	r3, [sp, #16]
00008658	e59d0004	ldr	r0, [sp, #4]
0000865c	eb000b21	bl	0xb2e8	; symbol stub for: __ZN6google8protobuf7MessageD2Ev
00008660	e59d3008	ldr	r3, [sp, #8]
00008664	e58d3000	str	r3, [sp]
00008668	e3e03000	mvn	r3, #0	; 0x0
0000866c	e58d3010	str	r3, [sp, #16]
00008670	e59d0000	ldr	r0, [sp]
00008674	eb000b00	bl	0xb27c	; symbol stub for: __Unwind_SjLj_Resume
00008678	e59d0040	ldr	r0, [sp, #64]
0000867c	eb000b31	bl	0xb348	; symbol stub for: __ZdlPv
00008680	e28d300c	add	r3, sp, #12	; 0xc
00008684	e1a00003	mov	r0, r3
00008688	eb000afe	bl	0xb288	; symbol stub for: __Unwind_SjLj_Unregister
0000868c	e247d05c	sub	sp, r7, #92	; 0x5c
00008690	ecbd8b11	fldmiax	sp!, {d8-d15}
00008694	e247d018	sub	sp, r7, #24	; 0x18
00008698	e8bd0d00	ldmia	sp!, {r8, r10, r11}
0000869c	e8bd80f0	ldmia	sp!, {r4, r5, r6, r7, pc}
000086a0	00003a74	andeq	r3, r0, r4, ror r10
000086a4	000040a6	andeq	r4, r0, r6, lsr #1
000086a8	00000068	andeq	r0, r0, r8, rrx
000086ac	00003e3c	andeq	r3, r0, ip, lsr lr
__ZN3dsp19LocationDescriptionD1Ev:
000086b0	e92d40f0	stmdb	sp!, {r4, r5, r6, r7, lr}
000086b4	e28d700c	add	r7, sp, #12	; 0xc
000086b8	e92d0d00	stmdb	sp!, {r8, r10, r11}
000086bc	ed2d8b11	fstmdbx	sp!, {d8-d15}
000086c0	e24dd044	sub	sp, sp, #68	; 0x44
000086c4	e58d0040	str	r0, [sp, #64]
000086c8	e59f30f8	ldr	r3, [pc, #248]	; 0x87c8
000086cc	e08f3003	add	r3, pc, r3
000086d0	e5933000	ldr	r3, [r3]
000086d4	e58d3024	str	r3, [sp, #36]
000086d8	e59f30ec	ldr	r3, [pc, #236]	; 0x87cc
000086dc	e08f3003	add	r3, pc, r3
000086e0	e58d3028	str	r3, [sp, #40]
000086e4	e28d202c	add	r2, sp, #44	; 0x2c
000086e8	e5827000	str	r7, [r2]
000086ec	e59f30dc	ldr	r3, [pc, #220]	; 0x87d0
000086f0	e08f3003	add	r3, pc, r3
000086f4	e5823004	str	r3, [r2, #4]
000086f8	e582d008	str	sp, [r2, #8]
000086fc	e28d300c	add	r3, sp, #12	; 0xc
00008700	e1a00003	mov	r0, r3
00008704	eb000ad9	bl	0xb270	; symbol stub for: __Unwind_SjLj_Register
00008708	e59f30c4	ldr	r3, [pc, #196]	; 0x87d4
0000870c	e08f3003	add	r3, pc, r3
00008710	e2832008	add	r2, r3, #8	; 0x8
00008714	e59d3040	ldr	r3, [sp, #64]
00008718	e5832000	str	r2, [r3]
0000871c	e59d3040	ldr	r3, [sp, #64]
00008720	e2832004	add	r2, r3, #4	; 0x4
00008724	e3a03001	mov	r3, #1	; 0x1
00008728	e58d3010	str	r3, [sp, #16]
0000872c	e1a00002	mov	r0, r2
00008730	eb000ae6	bl	0xb2d0	; symbol stub for: __ZN6google8protobuf15UnknownFieldSetD1Ev
00008734	e59d3040	ldr	r3, [sp, #64]
00008738	e58d3004	str	r3, [sp, #4]
0000873c	e3e03000	mvn	r3, #0	; 0x0
00008740	e58d3010	str	r3, [sp, #16]
00008744	e59d0004	ldr	r0, [sp, #4]
00008748	eb000ae6	bl	0xb2e8	; symbol stub for: __ZN6google8protobuf7MessageD2Ev
0000874c	e3a03000	mov	r3, #0	; 0x0
00008750	e6ef3073	uxtb r3,r3
00008754	e3530000	cmp	r3, #0	; 0x0
00008758	0a000012	beq	0x87a8
0000875c	ea00000f	b	0x87a0
00008760	e59d3014	ldr	r3, [sp, #20]
00008764	e58d3000	str	r3, [sp]
00008768	e59d3000	ldr	r3, [sp]
0000876c	e58d3008	str	r3, [sp, #8]
00008770	e59d3040	ldr	r3, [sp, #64]
00008774	e58d3004	str	r3, [sp, #4]
00008778	e3a03000	mov	r3, #0	; 0x0
0000877c	e58d3010	str	r3, [sp, #16]
00008780	e59d0004	ldr	r0, [sp, #4]
00008784	eb000ad7	bl	0xb2e8	; symbol stub for: __ZN6google8protobuf7MessageD2Ev
00008788	e59d3008	ldr	r3, [sp, #8]
0000878c	e58d3000	str	r3, [sp]
00008790	e3e03000	mvn	r3, #0	; 0x0
00008794	e58d3010	str	r3, [sp, #16]
00008798	e59d0000	ldr	r0, [sp]
0000879c	eb000ab6	bl	0xb27c	; symbol stub for: __Unwind_SjLj_Resume
000087a0	e59d0040	ldr	r0, [sp, #64]
000087a4	eb000ae7	bl	0xb348	; symbol stub for: __ZdlPv
000087a8	e28d300c	add	r3, sp, #12	; 0xc
000087ac	e1a00003	mov	r0, r3
000087b0	eb000ab4	bl	0xb288	; symbol stub for: __Unwind_SjLj_Unregister
000087b4	e247d05c	sub	sp, r7, #92	; 0x5c
000087b8	ecbd8b11	fldmiax	sp!, {d8-d15}
000087bc	e247d018	sub	sp, r7, #24	; 0x18
000087c0	e8bd0d00	ldmia	sp!, {r8, r10, r11}
000087c4	e8bd80f0	ldmia	sp!, {r4, r5, r6, r7, pc}
000087c8	0000394c	andeq	r3, r0, ip, asr #18
000087cc	00003f84	andeq	r3, r0, r4, lsl #31
000087d0	00000068	andeq	r0, r0, r8, rrx
000087d4	00003d14	andeq	r3, r0, r4, lsl sp
__ZN3dsp19LocationDescriptionD0Ev:
000087d8	e92d40f0	stmdb	sp!, {r4, r5, r6, r7, lr}
000087dc	e28d700c	add	r7, sp, #12	; 0xc
000087e0	e92d0d00	stmdb	sp!, {r8, r10, r11}
000087e4	ed2d8b11	fstmdbx	sp!, {d8-d15}
000087e8	e24dd044	sub	sp, sp, #68	; 0x44
000087ec	e58d0040	str	r0, [sp, #64]
000087f0	e59f30f8	ldr	r3, [pc, #248]	; 0x88f0
000087f4	e08f3003	add	r3, pc, r3
000087f8	e5933000	ldr	r3, [r3]
000087fc	e58d3024	str	r3, [sp, #36]
00008800	e59f30ec	ldr	r3, [pc, #236]	; 0x88f4
00008804	e08f3003	add	r3, pc, r3
00008808	e58d3028	str	r3, [sp, #40]
0000880c	e28d202c	add	r2, sp, #44	; 0x2c
00008810	e5827000	str	r7, [r2]
00008814	e59f30dc	ldr	r3, [pc, #220]	; 0x88f8
00008818	e08f3003	add	r3, pc, r3
0000881c	e5823004	str	r3, [r2, #4]
00008820	e582d008	str	sp, [r2, #8]
00008824	e28d300c	add	r3, sp, #12	; 0xc
00008828	e1a00003	mov	r0, r3
0000882c	eb000a8f	bl	0xb270	; symbol stub for: __Unwind_SjLj_Register
00008830	e59f30c4	ldr	r3, [pc, #196]	; 0x88fc
00008834	e08f3003	add	r3, pc, r3
00008838	e2832008	add	r2, r3, #8	; 0x8
0000883c	e59d3040	ldr	r3, [sp, #64]
00008840	e5832000	str	r2, [r3]
00008844	e59d3040	ldr	r3, [sp, #64]
00008848	e2832004	add	r2, r3, #4	; 0x4
0000884c	e3a03001	mov	r3, #1	; 0x1
00008850	e58d3010	str	r3, [sp, #16]
00008854	e1a00002	mov	r0, r2
00008858	eb000a9c	bl	0xb2d0	; symbol stub for: __ZN6google8protobuf15UnknownFieldSetD1Ev
0000885c	e59d3040	ldr	r3, [sp, #64]
00008860	e58d3004	str	r3, [sp, #4]
00008864	e3e03000	mvn	r3, #0	; 0x0
00008868	e58d3010	str	r3, [sp, #16]
0000886c	e59d0004	ldr	r0, [sp, #4]
00008870	eb000a9c	bl	0xb2e8	; symbol stub for: __ZN6google8protobuf7MessageD2Ev
00008874	e3a03001	mov	r3, #1	; 0x1
00008878	e6ef3073	uxtb r3,r3
0000887c	e3530000	cmp	r3, #0	; 0x0
00008880	0a000012	beq	0x88d0
00008884	ea00000f	b	0x88c8
00008888	e59d3014	ldr	r3, [sp, #20]
0000888c	e58d3000	str	r3, [sp]
00008890	e59d3000	ldr	r3, [sp]
00008894	e58d3008	str	r3, [sp, #8]
00008898	e59d3040	ldr	r3, [sp, #64]
0000889c	e58d3004	str	r3, [sp, #4]
000088a0	e3a03000	mov	r3, #0	; 0x0
000088a4	e58d3010	str	r3, [sp, #16]
000088a8	e59d0004	ldr	r0, [sp, #4]
000088ac	eb000a8d	bl	0xb2e8	; symbol stub for: __ZN6google8protobuf7MessageD2Ev
000088b0	e59d3008	ldr	r3, [sp, #8]
000088b4	e58d3000	str	r3, [sp]
000088b8	e3e03000	mvn	r3, #0	; 0x0
000088bc	e58d3010	str	r3, [sp, #16]
000088c0	e59d0000	ldr	r0, [sp]
000088c4	eb000a6c	bl	0xb27c	; symbol stub for: __Unwind_SjLj_Resume
000088c8	e59d0040	ldr	r0, [sp, #64]
000088cc	eb000a9d	bl	0xb348	; symbol stub for: __ZdlPv
000088d0	e28d300c	add	r3, sp, #12	; 0xc
000088d4	e1a00003	mov	r0, r3
000088d8	eb000a6a	bl	0xb288	; symbol stub for: __Unwind_SjLj_Unregister
000088dc	e247d05c	sub	sp, r7, #92	; 0x5c
000088e0	ecbd8b11	fldmiax	sp!, {d8-d15}
000088e4	e247d018	sub	sp, r7, #24	; 0x18
000088e8	e8bd0d00	ldmia	sp!, {r8, r10, r11}
000088ec	e8bd80f0	ldmia	sp!, {r4, r5, r6, r7, pc}
000088f0	00003824	andeq	r3, r0, r4, lsr #16
000088f4	00003e62	andeq	r3, r0, r2, ror #28
000088f8	00000068	andeq	r0, r0, r8, rrx
000088fc	00003bec	andeq	r3, r0, ip, ror #23
__ZN3dsp19LocationDescription10descriptorEv:
00008900	e92d4080	stmdb	sp!, {r7, lr}
00008904	e28d7000	add	r7, sp, #0	; 0x0
00008908	e59f3024	ldr	r3, [pc, #36]	; 0x8934
0000890c	e08f3003	add	r3, pc, r3
00008910	e5933000	ldr	r3, [r3]
00008914	e3530000	cmp	r3, #0	; 0x0
00008918	1a000000	bne	0x8920
0000891c	ebffeab3	bl	__ZN3dsp30protobuf_BuildDesc_dsp_2eprotoEv
00008920	e59f3010	ldr	r3, [pc, #16]	; 0x8938
00008924	e08f3003	add	r3, pc, r3
00008928	e5933000	ldr	r3, [r3]
0000892c	e1a00003	mov	r0, r3
00008930	e8bd8080	ldmia	sp!, {r7, pc}
00008934	000037e4	andeq	r3, r0, r4, ror #15
00008938	000037cc	andeq	r3, r0, ip, asr #15
__ZN3dsp19LocationDescription16default_instanceEv:
0000893c	e92d4080	stmdb	sp!, {r7, lr}
00008940	e28d7000	add	r7, sp, #0	; 0x0
00008944	e59f3024	ldr	r3, [pc, #36]	; 0x8970
00008948	e08f3003	add	r3, pc, r3
0000894c	e5933000	ldr	r3, [r3]
00008950	e3530000	cmp	r3, #0	; 0x0
00008954	1a000000	bne	0x895c
00008958	ebffeaa4	bl	__ZN3dsp30protobuf_BuildDesc_dsp_2eprotoEv
0000895c	e59f3010	ldr	r3, [pc, #16]	; 0x8974
00008960	e08f3003	add	r3, pc, r3
00008964	e5933000	ldr	r3, [r3]
00008968	e1a00003	mov	r0, r3
0000896c	e8bd8080	ldmia	sp!, {r7, pc}
00008970	00003780	andeq	r3, r0, r0, lsl #15
00008974	00003768	andeq	r3, r0, r8, ror #14
__ZNK3dsp19LocationDescription3NewEv:
00008978	e92d40f0	stmdb	sp!, {r4, r5, r6, r7, lr}
0000897c	e28d700c	add	r7, sp, #12	; 0xc
00008980	e92d0d00	stmdb	sp!, {r8, r10, r11}
00008984	ed2d8b11	fstmdbx	sp!, {d8-d15}
00008988	e24dd048	sub	sp, sp, #72	; 0x48
0000898c	e58d0044	str	r0, [sp, #68]
00008990	e59f30c0	ldr	r3, [pc, #192]	; 0x8a58
00008994	e08f3003	add	r3, pc, r3
00008998	e5933000	ldr	r3, [r3]
0000899c	e58d3028	str	r3, [sp, #40]
000089a0	e59f30b4	ldr	r3, [pc, #180]	; 0x8a5c
000089a4	e08f3003	add	r3, pc, r3
000089a8	e58d302c	str	r3, [sp, #44]
000089ac	e28d2030	add	r2, sp, #48	; 0x30
000089b0	e5827000	str	r7, [r2]
000089b4	e59f30a4	ldr	r3, [pc, #164]	; 0x8a60
000089b8	e08f3003	add	r3, pc, r3
000089bc	e5823004	str	r3, [r2, #4]
000089c0	e582d008	str	sp, [r2, #8]
000089c4	e28d3010	add	r3, sp, #16	; 0x10
000089c8	e1a00003	mov	r0, r3
000089cc	eb000a27	bl	0xb270	; symbol stub for: __Unwind_SjLj_Register
000089d0	e3e03000	mvn	r3, #0	; 0x0
000089d4	e58d3014	str	r3, [sp, #20]
000089d8	e3a00014	mov	r0, #20	; 0x14
000089dc	eb000a5c	bl	0xb354	; symbol stub for: __Znwm
000089e0	e1a03000	mov	r3, r0
000089e4	e58d3008	str	r3, [sp, #8]
000089e8	e3a03001	mov	r3, #1	; 0x1
000089ec	e58d3014	str	r3, [sp, #20]
000089f0	e59d0008	ldr	r0, [sp, #8]
000089f4	ebffeb2e	bl	__ZN3dsp19LocationDescriptionC1Ev
000089f8	ea00000b	b	0x8a2c
000089fc	e59d3018	ldr	r3, [sp, #24]
00008a00	e58d3000	str	r3, [sp]
00008a04	e59d3000	ldr	r3, [sp]
00008a08	e58d300c	str	r3, [sp, #12]
00008a0c	e59d0008	ldr	r0, [sp, #8]
00008a10	eb000a4c	bl	0xb348	; symbol stub for: __ZdlPv
00008a14	e59d300c	ldr	r3, [sp, #12]
00008a18	e58d3000	str	r3, [sp]
00008a1c	e3e03000	mvn	r3, #0	; 0x0
00008a20	e58d3014	str	r3, [sp, #20]
00008a24	e59d0000	ldr	r0, [sp]
00008a28	eb000a13	bl	0xb27c	; symbol stub for: __Unwind_SjLj_Resume
00008a2c	e59d3008	ldr	r3, [sp, #8]
00008a30	e58d3004	str	r3, [sp, #4]
00008a34	e28d3010	add	r3, sp, #16	; 0x10
00008a38	e1a00003	mov	r0, r3
00008a3c	eb000a11	bl	0xb288	; symbol stub for: __Unwind_SjLj_Unregister
00008a40	e59d0004	ldr	r0, [sp, #4]
00008a44	e247d05c	sub	sp, r7, #92	; 0x5c
00008a48	ecbd8b11	fldmiax	sp!, {d8-d15}
00008a4c	e247d018	sub	sp, r7, #24	; 0x18
00008a50	e8bd0d00	ldmia	sp!, {r8, r10, r11}
00008a54	e8bd80f0	ldmia	sp!, {r4, r5, r6, r7, pc}
00008a58	00003684	andeq	r3, r0, r4, lsl #13
00008a5c	00003cc8	andeq	r3, r0, r8, asr #25
00008a60	0000003c	andeq	r0, r0, ip, lsr r0
__ZNK3dsp19LocationDescription13GetDescriptorEv:
00008a64	e92d4080	stmdb	sp!, {r7, lr}
00008a68	e28d7000	add	r7, sp, #0	; 0x0
00008a6c	e24dd004	sub	sp, sp, #4	; 0x4
00008a70	e58d0000	str	r0, [sp]
00008a74	ebffffa1	bl	__ZN3dsp19LocationDescription10descriptorEv
00008a78	e1a03000	mov	r3, r0
00008a7c	e1a00003	mov	r0, r3
00008a80	e247d000	sub	sp, r7, #0	; 0x0
00008a84	e8bd8080	ldmia	sp!, {r7, pc}
__ZNK3dsp19LocationDescription13GetReflectionEv:
00008a88	e92d4080	stmdb	sp!, {r7, lr}
00008a8c	e28d7000	add	r7, sp, #0	; 0x0
00008a90	e24dd004	sub	sp, sp, #4	; 0x4
00008a94	e58d0000	str	r0, [sp]
00008a98	e59f3028	ldr	r3, [pc, #40]	; 0x8ac8
00008a9c	e08f3003	add	r3, pc, r3
00008aa0	e5933000	ldr	r3, [r3]
00008aa4	e3530000	cmp	r3, #0	; 0x0
00008aa8	1a000000	bne	0x8ab0
00008aac	ebffea4f	bl	__ZN3dsp30protobuf_BuildDesc_dsp_2eprotoEv
00008ab0	e59f3014	ldr	r3, [pc, #20]	; 0x8acc
00008ab4	e08f3003	add	r3, pc, r3
00008ab8	e5933000	ldr	r3, [r3]
00008abc	e1a00003	mov	r0, r3
00008ac0	e247d000	sub	sp, r7, #0	; 0x0
00008ac4	e8bd8080	ldmia	sp!, {r7, pc}
00008ac8	00003650	andeq	r3, r0, r0, asr r6
00008acc	00003638	andeq	r3, r0, r8, lsr r6
__ZN3dsp15LocationMessageC2Ev:
00008ad0	e92d40f0	stmdb	sp!, {r4, r5, r6, r7, lr}
00008ad4	e28d700c	add	r7, sp, #12	; 0xc
00008ad8	e92d0d00	stmdb	sp!, {r8, r10, r11}
00008adc	ed2d8b11	fstmdbx	sp!, {d8-d15}
00008ae0	e24dd040	sub	sp, sp, #64	; 0x40
00008ae4	e58d003c	str	r0, [sp, #60]
00008ae8	e59f3178	ldr	r3, [pc, #376]	; 0x8c68
00008aec	e08f3003	add	r3, pc, r3
00008af0	e5933000	ldr	r3, [r3]
00008af4	e58d3020	str	r3, [sp, #32]
00008af8	e59f316c	ldr	r3, [pc, #364]	; 0x8c6c
00008afc	e08f3003	add	r3, pc, r3
00008b00	e58d3024	str	r3, [sp, #36]
00008b04	e28d2028	add	r2, sp, #40	; 0x28
00008b08	e5827000	str	r7, [r2]
00008b0c	e59f315c	ldr	r3, [pc, #348]	; 0x8c70
00008b10	e08f3003	add	r3, pc, r3
00008b14	e5823004	str	r3, [r2, #4]
00008b18	e582d008	str	sp, [r2, #8]
00008b1c	e28d3008	add	r3, sp, #8	; 0x8
00008b20	e1a00003	mov	r0, r3
00008b24	eb0009d1	bl	0xb270	; symbol stub for: __Unwind_SjLj_Register
00008b28	e59d303c	ldr	r3, [sp, #60]
00008b2c	e1a00003	mov	r0, r3
00008b30	e59f313c	ldr	r3, [pc, #316]	; 0x8c74
00008b34	e08f3003	add	r3, pc, r3
00008b38	e5933000	ldr	r3, [r3]
00008b3c	e12fff33	blx	r3
00008b40	e59f3130	ldr	r3, [pc, #304]	; 0x8c78
00008b44	e08f3003	add	r3, pc, r3
00008b48	e2832008	add	r2, r3, #8	; 0x8
00008b4c	e59d303c	ldr	r3, [sp, #60]
00008b50	e5832000	str	r2, [r3]
00008b54	e59d303c	ldr	r3, [sp, #60]
00008b58	e2832004	add	r2, r3, #4	; 0x4
00008b5c	e3a03001	mov	r3, #1	; 0x1
00008b60	e58d300c	str	r3, [sp, #12]
00008b64	e1a00002	mov	r0, r2
00008b68	eb0009d5	bl	0xb2c4	; symbol stub for: __ZN6google8protobuf15UnknownFieldSetC1Ev
00008b6c	e59d203c	ldr	r2, [sp, #60]
00008b70	e3a03000	mov	r3, #0	; 0x0
00008b74	e5823008	str	r3, [r2, #8]
00008b78	e59d103c	ldr	r1, [sp, #60]
00008b7c	e3a02000	mov	r2, #0	; 0x0
00008b80	e3a03000	mov	r3, #0	; 0x0
00008b84	e581200c	str	r2, [r1, #12]
00008b88	e5813010	str	r3, [r1, #16]
00008b8c	e59d203c	ldr	r2, [sp, #60]
00008b90	e28f30c8	add	r3, pc, #200	; 0xc8
00008b94	e8930018	ldmia	r3, {r3, r4}
00008b98	e5823014	str	r3, [r2, #20]
00008b9c	e5824018	str	r4, [r2, #24]
00008ba0	e59d203c	ldr	r2, [sp, #60]
00008ba4	e28f30b4	add	r3, pc, #180	; 0xb4
00008ba8	e8930018	ldmia	r3, {r3, r4}
00008bac	e582301c	str	r3, [r2, #28]
00008bb0	e5824020	str	r4, [r2, #32]
00008bb4	e59d203c	ldr	r2, [sp, #60]
00008bb8	e28f30a0	add	r3, pc, #160	; 0xa0
00008bbc	e8930018	ldmia	r3, {r3, r4}
00008bc0	e5823024	str	r3, [r2, #36]
00008bc4	e5824028	str	r4, [r2, #40]
00008bc8	e59d203c	ldr	r2, [sp, #60]
00008bcc	e28f308c	add	r3, pc, #140	; 0x8c
00008bd0	e8930018	ldmia	r3, {r3, r4}
00008bd4	e582302c	str	r3, [r2, #44]
00008bd8	e5824030	str	r4, [r2, #48]
00008bdc	e59d203c	ldr	r2, [sp, #60]
00008be0	e28f3078	add	r3, pc, #120	; 0x78
00008be4	e8930018	ldmia	r3, {r3, r4}
00008be8	e5823034	str	r3, [r2, #52]
00008bec	e5824038	str	r4, [r2, #56]
00008bf0	e59d303c	ldr	r3, [sp, #60]
00008bf4	e283203c	add	r2, r3, #60	; 0x3c
00008bf8	e3a03000	mov	r3, #0	; 0x0
00008bfc	e5823000	str	r3, [r2]
00008c00	ea00000e	b	0x8c40
00008c04	e59d3010	ldr	r3, [sp, #16]
00008c08	e58d3000	str	r3, [sp]
00008c0c	e59d3000	ldr	r3, [sp]
00008c10	e58d3004	str	r3, [sp, #4]
00008c14	e59d203c	ldr	r2, [sp, #60]
00008c18	e3a03000	mov	r3, #0	; 0x0
00008c1c	e58d300c	str	r3, [sp, #12]
00008c20	e1a00002	mov	r0, r2
00008c24	eb0009af	bl	0xb2e8	; symbol stub for: __ZN6google8protobuf7MessageD2Ev
00008c28	e59d3004	ldr	r3, [sp, #4]
00008c2c	e58d3000	str	r3, [sp]
00008c30	e3e03000	mvn	r3, #0	; 0x0
00008c34	e58d300c	str	r3, [sp, #12]
00008c38	e59d0000	ldr	r0, [sp]
00008c3c	eb00098e	bl	0xb27c	; symbol stub for: __Unwind_SjLj_Resume
00008c40	e28d3008	add	r3, sp, #8	; 0x8
00008c44	e1a00003	mov	r0, r3
00008c48	eb00098e	bl	0xb288	; symbol stub for: __Unwind_SjLj_Unregister
00008c4c	e247d05c	sub	sp, r7, #92	; 0x5c
00008c50	ecbd8b11	fldmiax	sp!, {d8-d15}
00008c54	e247d018	sub	sp, r7, #24	; 0x18
00008c58	e8bd0d00	ldmia	sp!, {r8, r10, r11}
00008c5c	e8bd80f0	ldmia	sp!, {r4, r5, r6, r7, pc}
00008c60	00000000	andeq	r0, r0, r0
00008c64	00000000	andeq	r0, r0, r0
00008c68	0000352c	andeq	r3, r0, ip, lsr #10
00008c6c	00003b76	andeq	r3, r0, r6, ror r11
00008c70	000000ec	andeq	r0, r0, ip, ror #1
00008c74	000034ec	andeq	r3, r0, ip, ror #9
00008c78	00003924	andeq	r3, r0, r4, lsr #18
__ZN3dsp15LocationMessageC2ERKS0_:
00008c7c	e92d40f0	stmdb	sp!, {r4, r5, r6, r7, lr}
00008c80	e28d700c	add	r7, sp, #12	; 0xc
00008c84	e92d0d00	stmdb	sp!, {r8, r10, r11}
00008c88	ed2d8b11	fstmdbx	sp!, {d8-d15}
00008c8c	e24dd048	sub	sp, sp, #72	; 0x48
00008c90	e58d0044	str	r0, [sp, #68]
00008c94	e58d1040	str	r1, [sp, #64]
00008c98	e59f31c4	ldr	r3, [pc, #452]	; 0x8e64
00008c9c	e08f3003	add	r3, pc, r3
00008ca0	e5933000	ldr	r3, [r3]
00008ca4	e58d3024	str	r3, [sp, #36]
00008ca8	e59f31b8	ldr	r3, [pc, #440]	; 0x8e68
00008cac	e08f3003	add	r3, pc, r3
00008cb0	e58d3028	str	r3, [sp, #40]
00008cb4	e28d202c	add	r2, sp, #44	; 0x2c
00008cb8	e5827000	str	r7, [r2]
00008cbc	e59f31a8	ldr	r3, [pc, #424]	; 0x8e6c
00008cc0	e08f3003	add	r3, pc, r3
00008cc4	e5823004	str	r3, [r2, #4]
00008cc8	e582d008	str	sp, [r2, #8]
00008ccc	e28d300c	add	r3, sp, #12	; 0xc
00008cd0	e1a00003	mov	r0, r3
00008cd4	eb000965	bl	0xb270	; symbol stub for: __Unwind_SjLj_Register
00008cd8	e59d3044	ldr	r3, [sp, #68]
00008cdc	e1a00003	mov	r0, r3
00008ce0	e59f3188	ldr	r3, [pc, #392]	; 0x8e70
00008ce4	e08f3003	add	r3, pc, r3
00008ce8	e5933000	ldr	r3, [r3]
00008cec	e12fff33	blx	r3
00008cf0	e59f317c	ldr	r3, [pc, #380]	; 0x8e74
00008cf4	e08f3003	add	r3, pc, r3
00008cf8	e2832008	add	r2, r3, #8	; 0x8
00008cfc	e59d3044	ldr	r3, [sp, #68]
00008d00	e5832000	str	r2, [r3]
00008d04	e59d3044	ldr	r3, [sp, #68]
00008d08	e2832004	add	r2, r3, #4	; 0x4
00008d0c	e3a03002	mov	r3, #2	; 0x2
00008d10	e58d3010	str	r3, [sp, #16]
00008d14	e1a00002	mov	r0, r2
00008d18	eb000969	bl	0xb2c4	; symbol stub for: __ZN6google8protobuf15UnknownFieldSetC1Ev
00008d1c	e59d2044	ldr	r2, [sp, #68]
00008d20	e3a03000	mov	r3, #0	; 0x0
00008d24	e5823008	str	r3, [r2, #8]
00008d28	e59d1044	ldr	r1, [sp, #68]
00008d2c	e3a02000	mov	r2, #0	; 0x0
00008d30	e3a03000	mov	r3, #0	; 0x0
00008d34	e581200c	str	r2, [r1, #12]
00008d38	e5813010	str	r3, [r1, #16]
00008d3c	e59d2044	ldr	r2, [sp, #68]
00008d40	e28f3f45	add	r3, pc, #276	; 0x114
00008d44	e8930018	ldmia	r3, {r3, r4}
00008d48	e5823014	str	r3, [r2, #20]
00008d4c	e5824018	str	r4, [r2, #24]
00008d50	e59d2044	ldr	r2, [sp, #68]
00008d54	e28f3c01	add	r3, pc, #256	; 0x100
00008d58	e8930018	ldmia	r3, {r3, r4}
00008d5c	e582301c	str	r3, [r2, #28]
00008d60	e5824020	str	r4, [r2, #32]
00008d64	e59d2044	ldr	r2, [sp, #68]
00008d68	e28f30ec	add	r3, pc, #236	; 0xec
00008d6c	e8930018	ldmia	r3, {r3, r4}
00008d70	e5823024	str	r3, [r2, #36]
00008d74	e5824028	str	r4, [r2, #40]
00008d78	e59d2044	ldr	r2, [sp, #68]
00008d7c	e28f30d8	add	r3, pc, #216	; 0xd8
00008d80	e8930018	ldmia	r3, {r3, r4}
00008d84	e582302c	str	r3, [r2, #44]
00008d88	e5824030	str	r4, [r2, #48]
00008d8c	e59d2044	ldr	r2, [sp, #68]
00008d90	e28f30c4	add	r3, pc, #196	; 0xc4
00008d94	e8930018	ldmia	r3, {r3, r4}
00008d98	e5823034	str	r3, [r2, #52]
00008d9c	e5824038	str	r4, [r2, #56]
00008da0	e59d3044	ldr	r3, [sp, #68]
00008da4	e283203c	add	r2, r3, #60	; 0x3c
00008da8	e3a03000	mov	r3, #0	; 0x0
00008dac	e5823000	str	r3, [r2]
00008db0	e59d2044	ldr	r2, [sp, #68]
00008db4	e59d1040	ldr	r1, [sp, #64]
00008db8	e3a03001	mov	r3, #1	; 0x1
00008dbc	e58d3010	str	r3, [sp, #16]
00008dc0	e1a00002	mov	r0, r2
00008dc4	eb000944	bl	0xb2dc	; symbol stub for: __ZN6google8protobuf7Message9MergeFromERKS1_
00008dc8	ea00001b	b	0x8e3c
00008dcc	e59d3010	ldr	r3, [sp, #16]
00008dd0	e59d2014	ldr	r2, [sp, #20]
00008dd4	e58d2000	str	r2, [sp]
00008dd8	e3530001	cmp	r3, #1	; 0x1
00008ddc	0a000009	beq	0x8e08
00008de0	e59d3000	ldr	r3, [sp]
00008de4	e58d3004	str	r3, [sp, #4]
00008de8	e59d3044	ldr	r3, [sp, #68]
00008dec	e2832004	add	r2, r3, #4	; 0x4
00008df0	e3a03000	mov	r3, #0	; 0x0
00008df4	e58d3010	str	r3, [sp, #16]
00008df8	e1a00002	mov	r0, r2
00008dfc	eb000933	bl	0xb2d0	; symbol stub for: __ZN6google8protobuf15UnknownFieldSetD1Ev
00008e00	e59d2004	ldr	r2, [sp, #4]
00008e04	e58d2000	str	r2, [sp]
00008e08	e59d3000	ldr	r3, [sp]
00008e0c	e58d3008	str	r3, [sp, #8]
00008e10	e59d2044	ldr	r2, [sp, #68]
00008e14	e3a03000	mov	r3, #0	; 0x0
00008e18	e58d3010	str	r3, [sp, #16]
00008e1c	e1a00002	mov	r0, r2
00008e20	eb000930	bl	0xb2e8	; symbol stub for: __ZN6google8protobuf7MessageD2Ev
00008e24	e59d2008	ldr	r2, [sp, #8]
00008e28	e58d2000	str	r2, [sp]
00008e2c	e3e03000	mvn	r3, #0	; 0x0
00008e30	e58d3010	str	r3, [sp, #16]
00008e34	e59d0000	ldr	r0, [sp]
00008e38	eb00090f	bl	0xb27c	; symbol stub for: __Unwind_SjLj_Resume
00008e3c	e28d300c	add	r3, sp, #12	; 0xc
00008e40	e1a00003	mov	r0, r3
00008e44	eb00090f	bl	0xb288	; symbol stub for: __Unwind_SjLj_Unregister
00008e48	e247d05c	sub	sp, r7, #92	; 0x5c
00008e4c	ecbd8b11	fldmiax	sp!, {d8-d15}
00008e50	e247d018	sub	sp, r7, #24	; 0x18
00008e54	e8bd0d00	ldmia	sp!, {r8, r10, r11}
00008e58	e8bd80f0	ldmia	sp!, {r4, r5, r6, r7, pc}
00008e5c	00000000	andeq	r0, r0, r0
00008e60	00000000	andeq	r0, r0, r0
00008e64	0000337c	andeq	r3, r0, ip, ror r3
00008e68	000039cc	andeq	r3, r0, ip, asr #19
00008e6c	00000104	andeq	r0, r0, r4, lsl #2
00008e70	0000333c	andeq	r3, r0, ip, lsr r3
00008e74	00003774	andeq	r3, r0, r4, ror r7
__ZN3dsp15LocationMessageC1ERKS0_:
00008e78	e92d40f0	stmdb	sp!, {r4, r5, r6, r7, lr}
00008e7c	e28d700c	add	r7, sp, #12	; 0xc
00008e80	e92d0d00	stmdb	sp!, {r8, r10, r11}
00008e84	ed2d8b11	fstmdbx	sp!, {d8-d15}
00008e88	e24dd048	sub	sp, sp, #72	; 0x48
00008e8c	e58d0044	str	r0, [sp, #68]
00008e90	e58d1040	str	r1, [sp, #64]
00008e94	e59f31c4	ldr	r3, [pc, #452]	; 0x9060
00008e98	e08f3003	add	r3, pc, r3
00008e9c	e5933000	ldr	r3, [r3]
00008ea0	e58d3024	str	r3, [sp, #36]
00008ea4	e59f31b8	ldr	r3, [pc, #440]	; 0x9064
00008ea8	e08f3003	add	r3, pc, r3
00008eac	e58d3028	str	r3, [sp, #40]
00008eb0	e28d202c	add	r2, sp, #44	; 0x2c
00008eb4	e5827000	str	r7, [r2]
00008eb8	e59f31a8	ldr	r3, [pc, #424]	; 0x9068
00008ebc	e08f3003	add	r3, pc, r3
00008ec0	e5823004	str	r3, [r2, #4]
00008ec4	e582d008	str	sp, [r2, #8]
00008ec8	e28d300c	add	r3, sp, #12	; 0xc
00008ecc	e1a00003	mov	r0, r3
00008ed0	eb0008e6	bl	0xb270	; symbol stub for: __Unwind_SjLj_Register
00008ed4	e59d3044	ldr	r3, [sp, #68]
00008ed8	e1a00003	mov	r0, r3
00008edc	e59f3188	ldr	r3, [pc, #392]	; 0x906c
00008ee0	e08f3003	add	r3, pc, r3
00008ee4	e5933000	ldr	r3, [r3]
00008ee8	e12fff33	blx	r3
00008eec	e59f317c	ldr	r3, [pc, #380]	; 0x9070
00008ef0	e08f3003	add	r3, pc, r3
00008ef4	e2832008	add	r2, r3, #8	; 0x8
00008ef8	e59d3044	ldr	r3, [sp, #68]
00008efc	e5832000	str	r2, [r3]
00008f00	e59d3044	ldr	r3, [sp, #68]
00008f04	e2832004	add	r2, r3, #4	; 0x4
00008f08	e3a03002	mov	r3, #2	; 0x2
00008f0c	e58d3010	str	r3, [sp, #16]
00008f10	e1a00002	mov	r0, r2
00008f14	eb0008ea	bl	0xb2c4	; symbol stub for: __ZN6google8protobuf15UnknownFieldSetC1Ev
00008f18	e59d2044	ldr	r2, [sp, #68]
00008f1c	e3a03000	mov	r3, #0	; 0x0
00008f20	e5823008	str	r3, [r2, #8]
00008f24	e59d1044	ldr	r1, [sp, #68]
00008f28	e3a02000	mov	r2, #0	; 0x0
00008f2c	e3a03000	mov	r3, #0	; 0x0
00008f30	e581200c	str	r2, [r1, #12]
00008f34	e5813010	str	r3, [r1, #16]
00008f38	e59d2044	ldr	r2, [sp, #68]
00008f3c	e28f3f45	add	r3, pc, #276	; 0x114
00008f40	e8930018	ldmia	r3, {r3, r4}
00008f44	e5823014	str	r3, [r2, #20]
00008f48	e5824018	str	r4, [r2, #24]
00008f4c	e59d2044	ldr	r2, [sp, #68]
00008f50	e28f3c01	add	r3, pc, #256	; 0x100
00008f54	e8930018	ldmia	r3, {r3, r4}
00008f58	e582301c	str	r3, [r2, #28]
00008f5c	e5824020	str	r4, [r2, #32]
00008f60	e59d2044	ldr	r2, [sp, #68]
00008f64	e28f30ec	add	r3, pc, #236	; 0xec
00008f68	e8930018	ldmia	r3, {r3, r4}
00008f6c	e5823024	str	r3, [r2, #36]
00008f70	e5824028	str	r4, [r2, #40]
00008f74	e59d2044	ldr	r2, [sp, #68]
00008f78	e28f30d8	add	r3, pc, #216	; 0xd8
00008f7c	e8930018	ldmia	r3, {r3, r4}
00008f80	e582302c	str	r3, [r2, #44]
00008f84	e5824030	str	r4, [r2, #48]
00008f88	e59d2044	ldr	r2, [sp, #68]
00008f8c	e28f30c4	add	r3, pc, #196	; 0xc4
00008f90	e8930018	ldmia	r3, {r3, r4}
00008f94	e5823034	str	r3, [r2, #52]
00008f98	e5824038	str	r4, [r2, #56]
00008f9c	e59d3044	ldr	r3, [sp, #68]
00008fa0	e283203c	add	r2, r3, #60	; 0x3c
00008fa4	e3a03000	mov	r3, #0	; 0x0
00008fa8	e5823000	str	r3, [r2]
00008fac	e59d2044	ldr	r2, [sp, #68]
00008fb0	e59d1040	ldr	r1, [sp, #64]
00008fb4	e3a03001	mov	r3, #1	; 0x1
00008fb8	e58d3010	str	r3, [sp, #16]
00008fbc	e1a00002	mov	r0, r2
00008fc0	eb0008c5	bl	0xb2dc	; symbol stub for: __ZN6google8protobuf7Message9MergeFromERKS1_
00008fc4	ea00001b	b	0x9038
00008fc8	e59d3010	ldr	r3, [sp, #16]
00008fcc	e59d2014	ldr	r2, [sp, #20]
00008fd0	e58d2000	str	r2, [sp]
00008fd4	e3530001	cmp	r3, #1	; 0x1
00008fd8	0a000009	beq	0x9004
00008fdc	e59d3000	ldr	r3, [sp]
00008fe0	e58d3004	str	r3, [sp, #4]
00008fe4	e59d3044	ldr	r3, [sp, #68]
00008fe8	e2832004	add	r2, r3, #4	; 0x4
00008fec	e3a03000	mov	r3, #0	; 0x0
00008ff0	e58d3010	str	r3, [sp, #16]
00008ff4	e1a00002	mov	r0, r2
00008ff8	eb0008b4	bl	0xb2d0	; symbol stub for: __ZN6google8protobuf15UnknownFieldSetD1Ev
00008ffc	e59d2004	ldr	r2, [sp, #4]
00009000	e58d2000	str	r2, [sp]
00009004	e59d3000	ldr	r3, [sp]
00009008	e58d3008	str	r3, [sp, #8]
0000900c	e59d2044	ldr	r2, [sp, #68]
00009010	e3a03000	mov	r3, #0	; 0x0
00009014	e58d3010	str	r3, [sp, #16]
00009018	e1a00002	mov	r0, r2
0000901c	eb0008b1	bl	0xb2e8	; symbol stub for: __ZN6google8protobuf7MessageD2Ev
00009020	e59d2008	ldr	r2, [sp, #8]
00009024	e58d2000	str	r2, [sp]
00009028	e3e03000	mvn	r3, #0	; 0x0
0000902c	e58d3010	str	r3, [sp, #16]
00009030	e59d0000	ldr	r0, [sp]
00009034	eb000890	bl	0xb27c	; symbol stub for: __Unwind_SjLj_Resume
00009038	e28d300c	add	r3, sp, #12	; 0xc
0000903c	e1a00003	mov	r0, r3
00009040	eb000890	bl	0xb288	; symbol stub for: __Unwind_SjLj_Unregister
00009044	e247d05c	sub	sp, r7, #92	; 0x5c
00009048	ecbd8b11	fldmiax	sp!, {d8-d15}
0000904c	e247d018	sub	sp, r7, #24	; 0x18
00009050	e8bd0d00	ldmia	sp!, {r8, r10, r11}
00009054	e8bd80f0	ldmia	sp!, {r4, r5, r6, r7, pc}
00009058	00000000	andeq	r0, r0, r0
0000905c	00000000	andeq	r0, r0, r0
00009060	00003180	andeq	r3, r0, r0, lsl #3
00009064	000037d8	ldreqd	r3, [r0], -r8
00009068	00000104	andeq	r0, r0, r4, lsl #2
0000906c	00003140	andeq	r3, r0, r0, asr #2
00009070	00003578	andeq	r3, r0, r8, ror r5
__ZN3dsp15LocationMessageD2Ev:
00009074	e92d40f0	stmdb	sp!, {r4, r5, r6, r7, lr}
00009078	e28d700c	add	r7, sp, #12	; 0xc
0000907c	e92d0d00	stmdb	sp!, {r8, r10, r11}
00009080	ed2d8b11	fstmdbx	sp!, {d8-d15}
00009084	e24dd044	sub	sp, sp, #68	; 0x44
00009088	e58d0040	str	r0, [sp, #64]
0000908c	e59f30f8	ldr	r3, [pc, #248]	; 0x918c
00009090	e08f3003	add	r3, pc, r3
00009094	e5933000	ldr	r3, [r3]
00009098	e58d3024	str	r3, [sp, #36]
0000909c	e59f30ec	ldr	r3, [pc, #236]	; 0x9190
000090a0	e08f3003	add	r3, pc, r3
000090a4	e58d3028	str	r3, [sp, #40]
000090a8	e28d202c	add	r2, sp, #44	; 0x2c
000090ac	e5827000	str	r7, [r2]
000090b0	e59f30dc	ldr	r3, [pc, #220]	; 0x9194
000090b4	e08f3003	add	r3, pc, r3
000090b8	e5823004	str	r3, [r2, #4]
000090bc	e582d008	str	sp, [r2, #8]
000090c0	e28d300c	add	r3, sp, #12	; 0xc
000090c4	e1a00003	mov	r0, r3
000090c8	eb000868	bl	0xb270	; symbol stub for: __Unwind_SjLj_Register
000090cc	e59f30c4	ldr	r3, [pc, #196]	; 0x9198
000090d0	e08f3003	add	r3, pc, r3
000090d4	e2832008	add	r2, r3, #8	; 0x8
000090d8	e59d3040	ldr	r3, [sp, #64]
000090dc	e5832000	str	r2, [r3]
000090e0	e59d3040	ldr	r3, [sp, #64]
000090e4	e2832004	add	r2, r3, #4	; 0x4
000090e8	e3a03001	mov	r3, #1	; 0x1
000090ec	e58d3010	str	r3, [sp, #16]
000090f0	e1a00002	mov	r0, r2
000090f4	eb000875	bl	0xb2d0	; symbol stub for: __ZN6google8protobuf15UnknownFieldSetD1Ev
000090f8	e59d3040	ldr	r3, [sp, #64]
000090fc	e58d3004	str	r3, [sp, #4]
00009100	e3e03000	mvn	r3, #0	; 0x0
00009104	e58d3010	str	r3, [sp, #16]
00009108	e59d0004	ldr	r0, [sp, #4]
0000910c	eb000875	bl	0xb2e8	; symbol stub for: __ZN6google8protobuf7MessageD2Ev
00009110	e3a03000	mov	r3, #0	; 0x0
00009114	e6ef3073	uxtb r3,r3
00009118	e3530000	cmp	r3, #0	; 0x0
0000911c	0a000012	beq	0x916c
00009120	ea00000f	b	0x9164
00009124	e59d3014	ldr	r3, [sp, #20]
00009128	e58d3000	str	r3, [sp]
0000912c	e59d3000	ldr	r3, [sp]
00009130	e58d3008	str	r3, [sp, #8]
00009134	e59d3040	ldr	r3, [sp, #64]
00009138	e58d3004	str	r3, [sp, #4]
0000913c	e3a03000	mov	r3, #0	; 0x0
00009140	e58d3010	str	r3, [sp, #16]
00009144	e59d0004	ldr	r0, [sp, #4]
00009148	eb000866	bl	0xb2e8	; symbol stub for: __ZN6google8protobuf7MessageD2Ev
0000914c	e59d3008	ldr	r3, [sp, #8]
00009150	e58d3000	str	r3, [sp]
00009154	e3e03000	mvn	r3, #0	; 0x0
00009158	e58d3010	str	r3, [sp, #16]
0000915c	e59d0000	ldr	r0, [sp]
00009160	eb000845	bl	0xb27c	; symbol stub for: __Unwind_SjLj_Resume
00009164	e59d0040	ldr	r0, [sp, #64]
00009168	eb000876	bl	0xb348	; symbol stub for: __ZdlPv
0000916c	e28d300c	add	r3, sp, #12	; 0xc
00009170	e1a00003	mov	r0, r3
00009174	eb000843	bl	0xb288	; symbol stub for: __Unwind_SjLj_Unregister
00009178	e247d05c	sub	sp, r7, #92	; 0x5c
0000917c	ecbd8b11	fldmiax	sp!, {d8-d15}
00009180	e247d018	sub	sp, r7, #24	; 0x18
00009184	e8bd0d00	ldmia	sp!, {r8, r10, r11}
00009188	e8bd80f0	ldmia	sp!, {r4, r5, r6, r7, pc}
0000918c	00002f88	andeq	r2, r0, r8, lsl #31
00009190	000035e8	andeq	r3, r0, r8, ror #11
00009194	00000068	andeq	r0, r0, r8, rrx
00009198	00003398	muleq	r0, r8, r3
__ZN3dsp15LocationMessageD1Ev:
0000919c	e92d40f0	stmdb	sp!, {r4, r5, r6, r7, lr}
000091a0	e28d700c	add	r7, sp, #12	; 0xc
000091a4	e92d0d00	stmdb	sp!, {r8, r10, r11}
000091a8	ed2d8b11	fstmdbx	sp!, {d8-d15}
000091ac	e24dd044	sub	sp, sp, #68	; 0x44
000091b0	e58d0040	str	r0, [sp, #64]
000091b4	e59f30f8	ldr	r3, [pc, #248]	; 0x92b4
000091b8	e08f3003	add	r3, pc, r3
000091bc	e5933000	ldr	r3, [r3]
000091c0	e58d3024	str	r3, [sp, #36]
000091c4	e59f30ec	ldr	r3, [pc, #236]	; 0x92b8
000091c8	e08f3003	add	r3, pc, r3
000091cc	e58d3028	str	r3, [sp, #40]
000091d0	e28d202c	add	r2, sp, #44	; 0x2c
000091d4	e5827000	str	r7, [r2]
000091d8	e59f30dc	ldr	r3, [pc, #220]	; 0x92bc
000091dc	e08f3003	add	r3, pc, r3
000091e0	e5823004	str	r3, [r2, #4]
000091e4	e582d008	str	sp, [r2, #8]
000091e8	e28d300c	add	r3, sp, #12	; 0xc
000091ec	e1a00003	mov	r0, r3
000091f0	eb00081e	bl	0xb270	; symbol stub for: __Unwind_SjLj_Register
000091f4	e59f30c4	ldr	r3, [pc, #196]	; 0x92c0
000091f8	e08f3003	add	r3, pc, r3
000091fc	e2832008	add	r2, r3, #8	; 0x8
00009200	e59d3040	ldr	r3, [sp, #64]
00009204	e5832000	str	r2, [r3]
00009208	e59d3040	ldr	r3, [sp, #64]
0000920c	e2832004	add	r2, r3, #4	; 0x4
00009210	e3a03001	mov	r3, #1	; 0x1
00009214	e58d3010	str	r3, [sp, #16]
00009218	e1a00002	mov	r0, r2
0000921c	eb00082b	bl	0xb2d0	; symbol stub for: __ZN6google8protobuf15UnknownFieldSetD1Ev
00009220	e59d3040	ldr	r3, [sp, #64]
00009224	e58d3004	str	r3, [sp, #4]
00009228	e3e03000	mvn	r3, #0	; 0x0
0000922c	e58d3010	str	r3, [sp, #16]
00009230	e59d0004	ldr	r0, [sp, #4]
00009234	eb00082b	bl	0xb2e8	; symbol stub for: __ZN6google8protobuf7MessageD2Ev
00009238	e3a03000	mov	r3, #0	; 0x0
0000923c	e6ef3073	uxtb r3,r3
00009240	e3530000	cmp	r3, #0	; 0x0
00009244	0a000012	beq	0x9294
00009248	ea00000f	b	0x928c
0000924c	e59d3014	ldr	r3, [sp, #20]
00009250	e58d3000	str	r3, [sp]
00009254	e59d3000	ldr	r3, [sp]
00009258	e58d3008	str	r3, [sp, #8]
0000925c	e59d3040	ldr	r3, [sp, #64]
00009260	e58d3004	str	r3, [sp, #4]
00009264	e3a03000	mov	r3, #0	; 0x0
00009268	e58d3010	str	r3, [sp, #16]
0000926c	e59d0004	ldr	r0, [sp, #4]
00009270	eb00081c	bl	0xb2e8	; symbol stub for: __ZN6google8protobuf7MessageD2Ev
00009274	e59d3008	ldr	r3, [sp, #8]
00009278	e58d3000	str	r3, [sp]
0000927c	e3e03000	mvn	r3, #0	; 0x0
00009280	e58d3010	str	r3, [sp, #16]
00009284	e59d0000	ldr	r0, [sp]
00009288	eb0007fb	bl	0xb27c	; symbol stub for: __Unwind_SjLj_Resume
0000928c	e59d0040	ldr	r0, [sp, #64]
00009290	eb00082c	bl	0xb348	; symbol stub for: __ZdlPv
00009294	e28d300c	add	r3, sp, #12	; 0xc
00009298	e1a00003	mov	r0, r3
0000929c	eb0007f9	bl	0xb288	; symbol stub for: __Unwind_SjLj_Unregister
000092a0	e247d05c	sub	sp, r7, #92	; 0x5c
000092a4	ecbd8b11	fldmiax	sp!, {d8-d15}
000092a8	e247d018	sub	sp, r7, #24	; 0x18
000092ac	e8bd0d00	ldmia	sp!, {r8, r10, r11}
000092b0	e8bd80f0	ldmia	sp!, {r4, r5, r6, r7, pc}
000092b4	00002e60	andeq	r2, r0, r0, ror #28
000092b8	000034c6	andeq	r3, r0, r6, asr #9
000092bc	00000068	andeq	r0, r0, r8, rrx
000092c0	00003270	andeq	r3, r0, r0, ror r2
__ZN3dsp15LocationMessageD0Ev:
000092c4	e92d40f0	stmdb	sp!, {r4, r5, r6, r7, lr}
000092c8	e28d700c	add	r7, sp, #12	; 0xc
000092cc	e92d0d00	stmdb	sp!, {r8, r10, r11}
000092d0	ed2d8b11	fstmdbx	sp!, {d8-d15}
000092d4	e24dd044	sub	sp, sp, #68	; 0x44
000092d8	e58d0040	str	r0, [sp, #64]
000092dc	e59f30f8	ldr	r3, [pc, #248]	; 0x93dc
000092e0	e08f3003	add	r3, pc, r3
000092e4	e5933000	ldr	r3, [r3]
000092e8	e58d3024	str	r3, [sp, #36]
000092ec	e59f30ec	ldr	r3, [pc, #236]	; 0x93e0
000092f0	e08f3003	add	r3, pc, r3
000092f4	e58d3028	str	r3, [sp, #40]
000092f8	e28d202c	add	r2, sp, #44	; 0x2c
000092fc	e5827000	str	r7, [r2]
00009300	e59f30dc	ldr	r3, [pc, #220]	; 0x93e4
00009304	e08f3003	add	r3, pc, r3
00009308	e5823004	str	r3, [r2, #4]
0000930c	e582d008	str	sp, [r2, #8]
00009310	e28d300c	add	r3, sp, #12	; 0xc
00009314	e1a00003	mov	r0, r3
00009318	eb0007d4	bl	0xb270	; symbol stub for: __Unwind_SjLj_Register
0000931c	e59f30c4	ldr	r3, [pc, #196]	; 0x93e8
00009320	e08f3003	add	r3, pc, r3
00009324	e2832008	add	r2, r3, #8	; 0x8
00009328	e59d3040	ldr	r3, [sp, #64]
0000932c	e5832000	str	r2, [r3]
00009330	e59d3040	ldr	r3, [sp, #64]
00009334	e2832004	add	r2, r3, #4	; 0x4
00009338	e3a03001	mov	r3, #1	; 0x1
0000933c	e58d3010	str	r3, [sp, #16]
00009340	e1a00002	mov	r0, r2
00009344	eb0007e1	bl	0xb2d0	; symbol stub for: __ZN6google8protobuf15UnknownFieldSetD1Ev
00009348	e59d3040	ldr	r3, [sp, #64]
0000934c	e58d3004	str	r3, [sp, #4]
00009350	e3e03000	mvn	r3, #0	; 0x0
00009354	e58d3010	str	r3, [sp, #16]
00009358	e59d0004	ldr	r0, [sp, #4]
0000935c	eb0007e1	bl	0xb2e8	; symbol stub for: __ZN6google8protobuf7MessageD2Ev
00009360	e3a03001	mov	r3, #1	; 0x1
00009364	e6ef3073	uxtb r3,r3
00009368	e3530000	cmp	r3, #0	; 0x0
0000936c	0a000012	beq	0x93bc
00009370	ea00000f	b	0x93b4
00009374	e59d3014	ldr	r3, [sp, #20]
00009378	e58d3000	str	r3, [sp]
0000937c	e59d3000	ldr	r3, [sp]
00009380	e58d3008	str	r3, [sp, #8]
00009384	e59d3040	ldr	r3, [sp, #64]
00009388	e58d3004	str	r3, [sp, #4]
0000938c	e3a03000	mov	r3, #0	; 0x0
00009390	e58d3010	str	r3, [sp, #16]
00009394	e59d0004	ldr	r0, [sp, #4]
00009398	eb0007d2	bl	0xb2e8	; symbol stub for: __ZN6google8protobuf7MessageD2Ev
0000939c	e59d3008	ldr	r3, [sp, #8]
000093a0	e58d3000	str	r3, [sp]
000093a4	e3e03000	mvn	r3, #0	; 0x0
000093a8	e58d3010	str	r3, [sp, #16]
000093ac	e59d0000	ldr	r0, [sp]
000093b0	eb0007b1	bl	0xb27c	; symbol stub for: __Unwind_SjLj_Resume
000093b4	e59d0040	ldr	r0, [sp, #64]
000093b8	eb0007e2	bl	0xb348	; symbol stub for: __ZdlPv
000093bc	e28d300c	add	r3, sp, #12	; 0xc
000093c0	e1a00003	mov	r0, r3
000093c4	eb0007af	bl	0xb288	; symbol stub for: __Unwind_SjLj_Unregister
000093c8	e247d05c	sub	sp, r7, #92	; 0x5c
000093cc	ecbd8b11	fldmiax	sp!, {d8-d15}
000093d0	e247d018	sub	sp, r7, #24	; 0x18
000093d4	e8bd0d00	ldmia	sp!, {r8, r10, r11}
000093d8	e8bd80f0	ldmia	sp!, {r4, r5, r6, r7, pc}
000093dc	00002d38	andeq	r2, r0, r8, lsr sp
000093e0	000033a4	andeq	r3, r0, r4, lsr #7
000093e4	00000068	andeq	r0, r0, r8, rrx
000093e8	00003148	andeq	r3, r0, r8, asr #2
__ZN3dsp15LocationMessage10descriptorEv:
000093ec	e92d4080	stmdb	sp!, {r7, lr}
000093f0	e28d7000	add	r7, sp, #0	; 0x0
000093f4	e59f3024	ldr	r3, [pc, #36]	; 0x9420
000093f8	e08f3003	add	r3, pc, r3
000093fc	e5933000	ldr	r3, [r3]
00009400	e3530000	cmp	r3, #0	; 0x0
00009404	1a000000	bne	0x940c
00009408	ebffe7f8	bl	__ZN3dsp30protobuf_BuildDesc_dsp_2eprotoEv
0000940c	e59f3010	ldr	r3, [pc, #16]	; 0x9424
00009410	e08f3003	add	r3, pc, r3
00009414	e5933000	ldr	r3, [r3]
00009418	e1a00003	mov	r0, r3
0000941c	e8bd8080	ldmia	sp!, {r7, pc}
00009420	00002cf0	streqd	r2, [r0], -r0
00009424	00002cd8	ldreqd	r2, [r0], -r8
__ZN3dsp15LocationMessage16default_instanceEv:
00009428	e92d4080	stmdb	sp!, {r7, lr}
0000942c	e28d7000	add	r7, sp, #0	; 0x0
00009430	e59f3024	ldr	r3, [pc, #36]	; 0x945c
00009434	e08f3003	add	r3, pc, r3
00009438	e5933000	ldr	r3, [r3]
0000943c	e3530000	cmp	r3, #0	; 0x0
00009440	1a000000	bne	0x9448
00009444	ebffe7e9	bl	__ZN3dsp30protobuf_BuildDesc_dsp_2eprotoEv
00009448	e59f3010	ldr	r3, [pc, #16]	; 0x9460
0000944c	e08f3003	add	r3, pc, r3
00009450	e5933000	ldr	r3, [r3]
00009454	e1a00003	mov	r0, r3
00009458	e8bd8080	ldmia	sp!, {r7, pc}
0000945c	00002c90	muleq	r0, r0, ip
00009460	00002c78	andeq	r2, r0, r8, ror ip
__ZNK3dsp15LocationMessage3NewEv:
00009464	e92d40f0	stmdb	sp!, {r4, r5, r6, r7, lr}
00009468	e28d700c	add	r7, sp, #12	; 0xc
0000946c	e92d0d00	stmdb	sp!, {r8, r10, r11}
00009470	ed2d8b11	fstmdbx	sp!, {d8-d15}
00009474	e24dd048	sub	sp, sp, #72	; 0x48
00009478	e58d0044	str	r0, [sp, #68]
0000947c	e59f30c0	ldr	r3, [pc, #192]	; 0x9544
00009480	e08f3003	add	r3, pc, r3
00009484	e5933000	ldr	r3, [r3]
00009488	e58d3028	str	r3, [sp, #40]
0000948c	e59f30b4	ldr	r3, [pc, #180]	; 0x9548
00009490	e08f3003	add	r3, pc, r3
00009494	e58d302c	str	r3, [sp, #44]
00009498	e28d2030	add	r2, sp, #48	; 0x30
0000949c	e5827000	str	r7, [r2]
000094a0	e59f30a4	ldr	r3, [pc, #164]	; 0x954c
000094a4	e08f3003	add	r3, pc, r3
000094a8	e5823004	str	r3, [r2, #4]
000094ac	e582d008	str	sp, [r2, #8]
000094b0	e28d3010	add	r3, sp, #16	; 0x10
000094b4	e1a00003	mov	r0, r3
000094b8	eb00076c	bl	0xb270	; symbol stub for: __Unwind_SjLj_Register
000094bc	e3e03000	mvn	r3, #0	; 0x0
000094c0	e58d3014	str	r3, [sp, #20]
000094c4	e3a00040	mov	r0, #64	; 0x40
000094c8	eb0007a1	bl	0xb354	; symbol stub for: __Znwm
000094cc	e1a03000	mov	r3, r0
000094d0	e58d3008	str	r3, [sp, #8]
000094d4	e3a03001	mov	r3, #1	; 0x1
000094d8	e58d3014	str	r3, [sp, #20]
000094dc	e59d0008	ldr	r0, [sp, #8]
000094e0	ebffe808	bl	__ZN3dsp15LocationMessageC1Ev
000094e4	ea00000b	b	0x9518
000094e8	e59d3018	ldr	r3, [sp, #24]
000094ec	e58d3000	str	r3, [sp]
000094f0	e59d3000	ldr	r3, [sp]
000094f4	e58d300c	str	r3, [sp, #12]
000094f8	e59d0008	ldr	r0, [sp, #8]
000094fc	eb000791	bl	0xb348	; symbol stub for: __ZdlPv
00009500	e59d300c	ldr	r3, [sp, #12]
00009504	e58d3000	str	r3, [sp]
00009508	e3e03000	mvn	r3, #0	; 0x0
0000950c	e58d3014	str	r3, [sp, #20]
00009510	e59d0000	ldr	r0, [sp]
00009514	eb000758	bl	0xb27c	; symbol stub for: __Unwind_SjLj_Resume
00009518	e59d3008	ldr	r3, [sp, #8]
0000951c	e58d3004	str	r3, [sp, #4]
00009520	e28d3010	add	r3, sp, #16	; 0x10
00009524	e1a00003	mov	r0, r3
00009528	eb000756	bl	0xb288	; symbol stub for: __Unwind_SjLj_Unregister
0000952c	e59d0004	ldr	r0, [sp, #4]
00009530	e247d05c	sub	sp, r7, #92	; 0x5c
00009534	ecbd8b11	fldmiax	sp!, {d8-d15}
00009538	e247d018	sub	sp, r7, #24	; 0x18
0000953c	e8bd0d00	ldmia	sp!, {r8, r10, r11}
00009540	e8bd80f0	ldmia	sp!, {r4, r5, r6, r7, pc}
00009544	00002b98	muleq	r0, r8, r11
00009548	0000320a	andeq	r3, r0, r10, lsl #4
0000954c	0000003c	andeq	r0, r0, ip, lsr r0
__ZNK3dsp15LocationMessage13GetDescriptorEv:
00009550	e92d4080	stmdb	sp!, {r7, lr}
00009554	e28d7000	add	r7, sp, #0	; 0x0
00009558	e24dd004	sub	sp, sp, #4	; 0x4
0000955c	e58d0000	str	r0, [sp]
00009560	ebffffa1	bl	__ZN3dsp15LocationMessage10descriptorEv
00009564	e1a03000	mov	r3, r0
00009568	e1a00003	mov	r0, r3
0000956c	e247d000	sub	sp, r7, #0	; 0x0
00009570	e8bd8080	ldmia	sp!, {r7, pc}
__ZNK3dsp15LocationMessage13GetReflectionEv:
00009574	e92d4080	stmdb	sp!, {r7, lr}
00009578	e28d7000	add	r7, sp, #0	; 0x0
0000957c	e24dd004	sub	sp, sp, #4	; 0x4
00009580	e58d0000	str	r0, [sp]
00009584	e59f3028	ldr	r3, [pc, #40]	; 0x95b4
00009588	e08f3003	add	r3, pc, r3
0000958c	e5933000	ldr	r3, [r3]
00009590	e3530000	cmp	r3, #0	; 0x0
00009594	1a000000	bne	0x959c
00009598	ebffe794	bl	__ZN3dsp30protobuf_BuildDesc_dsp_2eprotoEv
0000959c	e59f3014	ldr	r3, [pc, #20]	; 0x95b8
000095a0	e08f3003	add	r3, pc, r3
000095a4	e5933000	ldr	r3, [r3]
000095a8	e1a00003	mov	r0, r3
000095ac	e247d000	sub	sp, r7, #0	; 0x0
000095b0	e8bd8080	ldmia	sp!, {r7, pc}
000095b4	00002b5c	andeq	r2, r0, ip, asr r11
000095b8	00002b44	andeq	r2, r0, r4, asr #22
___tcf_0:
000095bc	e92d4080	stmdb	sp!, {r7, lr}
000095c0	e28d7000	add	r7, sp, #0	; 0x0
000095c4	e24dd004	sub	sp, sp, #4	; 0x4
000095c8	e58d0000	str	r0, [sp]
000095cc	e59f3010	ldr	r3, [pc, #16]	; 0x95e4
000095d0	e08f3003	add	r3, pc, r3
000095d4	e1a00003	mov	r0, r3
000095d8	eb000757	bl	0xb33c	; symbol stub for: __ZNSsD1Ev
000095dc	e247d000	sub	sp, r7, #0	; 0x0
000095e0	e8bd8080	ldmia	sp!, {r7, pc}
000095e4	0000310c	andeq	r3, r0, ip, lsl #2
___tcf_1:
000095e8	e92d4080	stmdb	sp!, {r7, lr}
000095ec	e28d7000	add	r7, sp, #0	; 0x0
000095f0	e24dd004	sub	sp, sp, #4	; 0x4
000095f4	e58d0000	str	r0, [sp]
000095f8	e59f3010	ldr	r3, [pc, #16]	; 0x9610
000095fc	e08f3003	add	r3, pc, r3
00009600	e1a00003	mov	r0, r3
00009604	eb00074c	bl	0xb33c	; symbol stub for: __ZNSsD1Ev
00009608	e247d000	sub	sp, r7, #0	; 0x0
0000960c	e8bd8080	ldmia	sp!, {r7, pc}
00009610	000030dc	ldreqd	r3, [r0], -ip
__ZN6google8protobuf7MessageC2Ev:
00009614	e92d4080	stmdb	sp!, {r7, lr}
00009618	e28d7000	add	r7, sp, #0	; 0x0
0000961c	e24dd004	sub	sp, sp, #4	; 0x4
00009620	e58d0000	str	r0, [sp]
00009624	e59f3018	ldr	r3, [pc, #24]	; 0x9644
00009628	e08f3003	add	r3, pc, r3
0000962c	e5933000	ldr	r3, [r3]
00009630	e2832008	add	r2, r3, #8	; 0x8
00009634	e59d3000	ldr	r3, [sp]
00009638	e5832000	str	r2, [r3]
0000963c	e247d000	sub	sp, r7, #0	; 0x0
00009640	e8bd8080	ldmia	sp!, {r7, pc}
00009644	000029f4	streqd	r2, [r0], -r4
__ZNK3dsp6Header13GetCachedSizeEv:
00009648	e92d4080	stmdb	sp!, {r7, lr}
0000964c	e28d7000	add	r7, sp, #0	; 0x0
00009650	e24dd004	sub	sp, sp, #4	; 0x4
00009654	e58d0000	str	r0, [sp]
00009658	e59d3000	ldr	r3, [sp]
0000965c	e5933008	ldr	r3, [r3, #8]
00009660	e1a00003	mov	r0, r3
00009664	e247d000	sub	sp, r7, #0	; 0x0
00009668	e8bd8080	ldmia	sp!, {r7, pc}
__ZNK3dsp6Header13SetCachedSizeEi:
0000966c	e92d4080	stmdb	sp!, {r7, lr}
00009670	e28d7000	add	r7, sp, #0	; 0x0
00009674	e24dd008	sub	sp, sp, #8	; 0x8
00009678	e58d0004	str	r0, [sp, #4]
0000967c	e58d1000	str	r1, [sp]
00009680	e59d2004	ldr	r2, [sp, #4]
00009684	e59d3000	ldr	r3, [sp]
00009688	e5823008	str	r3, [r2, #8]
0000968c	e247d000	sub	sp, r7, #0	; 0x0
00009690	e8bd8080	ldmia	sp!, {r7, pc}
__ZNK3dsp24AccelerometerDescription13GetCachedSizeEv:
00009694	e92d4080	stmdb	sp!, {r7, lr}
00009698	e28d7000	add	r7, sp, #0	; 0x0
0000969c	e24dd004	sub	sp, sp, #4	; 0x4
000096a0	e58d0000	str	r0, [sp]
000096a4	e59d3000	ldr	r3, [sp]
000096a8	e5933008	ldr	r3, [r3, #8]
000096ac	e1a00003	mov	r0, r3
000096b0	e247d000	sub	sp, r7, #0	; 0x0
000096b4	e8bd8080	ldmia	sp!, {r7, pc}
__ZNK3dsp24AccelerometerDescription13SetCachedSizeEi:
000096b8	e92d4080	stmdb	sp!, {r7, lr}
000096bc	e28d7000	add	r7, sp, #0	; 0x0
000096c0	e24dd008	sub	sp, sp, #8	; 0x8
000096c4	e58d0004	str	r0, [sp, #4]
000096c8	e58d1000	str	r1, [sp]
000096cc	e59d2004	ldr	r2, [sp, #4]
000096d0	e59d3000	ldr	r3, [sp]
000096d4	e5823008	str	r3, [r2, #8]
000096d8	e247d000	sub	sp, r7, #0	; 0x0
000096dc	e8bd8080	ldmia	sp!, {r7, pc}
__ZNK3dsp20AccelerometerMessage13GetCachedSizeEv:
000096e0	e92d4080	stmdb	sp!, {r7, lr}
000096e4	e28d7000	add	r7, sp, #0	; 0x0
000096e8	e24dd004	sub	sp, sp, #4	; 0x4
000096ec	e58d0000	str	r0, [sp]
000096f0	e59d3000	ldr	r3, [sp]
000096f4	e5933008	ldr	r3, [r3, #8]
000096f8	e1a00003	mov	r0, r3
000096fc	e247d000	sub	sp, r7, #0	; 0x0
00009700	e8bd8080	ldmia	sp!, {r7, pc}
__ZNK3dsp20AccelerometerMessage13SetCachedSizeEi:
00009704	e92d4080	stmdb	sp!, {r7, lr}
00009708	e28d7000	add	r7, sp, #0	; 0x0
0000970c	e24dd008	sub	sp, sp, #8	; 0x8
00009710	e58d0004	str	r0, [sp, #4]
00009714	e58d1000	str	r1, [sp]
00009718	e59d2004	ldr	r2, [sp, #4]
0000971c	e59d3000	ldr	r3, [sp]
00009720	e5823008	str	r3, [r2, #8]
00009724	e247d000	sub	sp, r7, #0	; 0x0
00009728	e8bd8080	ldmia	sp!, {r7, pc}
__ZNK3dsp21MicrophoneDescription13GetCachedSizeEv:
0000972c	e92d4080	stmdb	sp!, {r7, lr}
00009730	e28d7000	add	r7, sp, #0	; 0x0
00009734	e24dd004	sub	sp, sp, #4	; 0x4
00009738	e58d0000	str	r0, [sp]
0000973c	e59d3000	ldr	r3, [sp]
00009740	e5933008	ldr	r3, [r3, #8]
00009744	e1a00003	mov	r0, r3
00009748	e247d000	sub	sp, r7, #0	; 0x0
0000974c	e8bd8080	ldmia	sp!, {r7, pc}
__ZNK3dsp21MicrophoneDescription13SetCachedSizeEi:
00009750	e92d4080	stmdb	sp!, {r7, lr}
00009754	e28d7000	add	r7, sp, #0	; 0x0
00009758	e24dd008	sub	sp, sp, #8	; 0x8
0000975c	e58d0004	str	r0, [sp, #4]
00009760	e58d1000	str	r1, [sp]
00009764	e59d2004	ldr	r2, [sp, #4]
00009768	e59d3000	ldr	r3, [sp]
0000976c	e5823008	str	r3, [r2, #8]
00009770	e247d000	sub	sp, r7, #0	; 0x0
00009774	e8bd8080	ldmia	sp!, {r7, pc}
__ZNK3dsp17MicrophoneMessage13GetCachedSizeEv:
00009778	e92d4080	stmdb	sp!, {r7, lr}
0000977c	e28d7000	add	r7, sp, #0	; 0x0
00009780	e24dd004	sub	sp, sp, #4	; 0x4
00009784	e58d0000	str	r0, [sp]
00009788	e59d3000	ldr	r3, [sp]
0000978c	e5933008	ldr	r3, [r3, #8]
00009790	e1a00003	mov	r0, r3
00009794	e247d000	sub	sp, r7, #0	; 0x0
00009798	e8bd8080	ldmia	sp!, {r7, pc}
__ZNK3dsp17MicrophoneMessage13SetCachedSizeEi:
0000979c	e92d4080	stmdb	sp!, {r7, lr}
000097a0	e28d7000	add	r7, sp, #0	; 0x0
000097a4	e24dd008	sub	sp, sp, #8	; 0x8
000097a8	e58d0004	str	r0, [sp, #4]
000097ac	e58d1000	str	r1, [sp]
000097b0	e59d2004	ldr	r2, [sp, #4]
000097b4	e59d3000	ldr	r3, [sp]
000097b8	e5823008	str	r3, [r2, #8]
000097bc	e247d000	sub	sp, r7, #0	; 0x0
000097c0	e8bd8080	ldmia	sp!, {r7, pc}
__ZNK3dsp19LocationDescription13GetCachedSizeEv:
000097c4	e92d4080	stmdb	sp!, {r7, lr}
000097c8	e28d7000	add	r7, sp, #0	; 0x0
000097cc	e24dd004	sub	sp, sp, #4	; 0x4
000097d0	e58d0000	str	r0, [sp]
000097d4	e59d3000	ldr	r3, [sp]
000097d8	e5933008	ldr	r3, [r3, #8]
000097dc	e1a00003	mov	r0, r3
000097e0	e247d000	sub	sp, r7, #0	; 0x0
000097e4	e8bd8080	ldmia	sp!, {r7, pc}
__ZNK3dsp19LocationDescription13SetCachedSizeEi:
000097e8	e92d4080	stmdb	sp!, {r7, lr}
000097ec	e28d7000	add	r7, sp, #0	; 0x0
000097f0	e24dd008	sub	sp, sp, #8	; 0x8
000097f4	e58d0004	str	r0, [sp, #4]
000097f8	e58d1000	str	r1, [sp]
000097fc	e59d2004	ldr	r2, [sp, #4]
00009800	e59d3000	ldr	r3, [sp]
00009804	e5823008	str	r3, [r2, #8]
00009808	e247d000	sub	sp, r7, #0	; 0x0
0000980c	e8bd8080	ldmia	sp!, {r7, pc}
__ZNK3dsp15LocationMessage13GetCachedSizeEv:
00009810	e92d4080	stmdb	sp!, {r7, lr}
00009814	e28d7000	add	r7, sp, #0	; 0x0
00009818	e24dd004	sub	sp, sp, #4	; 0x4
0000981c	e58d0000	str	r0, [sp]
00009820	e59d3000	ldr	r3, [sp]
00009824	e5933008	ldr	r3, [r3, #8]
00009828	e1a00003	mov	r0, r3
0000982c	e247d000	sub	sp, r7, #0	; 0x0
00009830	e8bd8080	ldmia	sp!, {r7, pc}
__ZNK3dsp15LocationMessage13SetCachedSizeEi:
00009834	e92d4080	stmdb	sp!, {r7, lr}
00009838	e28d7000	add	r7, sp, #0	; 0x0
0000983c	e24dd008	sub	sp, sp, #8	; 0x8
00009840	e58d0004	str	r0, [sp, #4]
00009844	e58d1000	str	r1, [sp]
00009848	e59d2004	ldr	r2, [sp, #4]
0000984c	e59d3000	ldr	r3, [sp]
00009850	e5823008	str	r3, [r2, #8]
00009854	e247d000	sub	sp, r7, #0	; 0x0
00009858	e8bd8080	ldmia	sp!, {r7, pc}
__ZNK6google8protobuf14FileDescriptor12message_typeEi:
0000985c	e92d4080	stmdb	sp!, {r7, lr}
00009860	e28d7000	add	r7, sp, #0	; 0x0
00009864	e24dd008	sub	sp, sp, #8	; 0x8
00009868	e58d0004	str	r0, [sp, #4]
0000986c	e58d1000	str	r1, [sp]
00009870	e59d3004	ldr	r3, [sp, #4]
00009874	e5931018	ldr	r1, [r3, #24]
00009878	e59d3000	ldr	r3, [sp]
0000987c	e1a02103	mov	r2, r3, lsl #2
00009880	e1a03202	mov	r3, r2, lsl #4
00009884	e0623003	rsb	r3, r2, r3
00009888	e0813003	add	r3, r1, r3
0000988c	e1a00003	mov	r0, r3
00009890	e247d000	sub	sp, r7, #0	; 0x0
00009894	e8bd8080	ldmia	sp!, {r7, pc}
__ZNK6google8protobuf14FileDescriptor9enum_typeEi:
00009898	e92d4080	stmdb	sp!, {r7, lr}
0000989c	e28d7000	add	r7, sp, #0	; 0x0
000098a0	e24dd008	sub	sp, sp, #8	; 0x8
000098a4	e58d0004	str	r0, [sp, #4]
000098a8	e58d1000	str	r1, [sp]
000098ac	e59d3004	ldr	r3, [sp, #4]
000098b0	e5931020	ldr	r1, [r3, #32]
000098b4	e59d3000	ldr	r3, [sp]
000098b8	e1a02103	mov	r2, r3, lsl #2
000098bc	e1a03182	mov	r3, r2, lsl #3
000098c0	e0623003	rsb	r3, r2, r3
000098c4	e0813003	add	r3, r1, r3
000098c8	e1a00003	mov	r0, r3
000098cc	e247d000	sub	sp, r7, #0	; 0x0
000098d0	e8bd8080	ldmia	sp!, {r7, pc}
__ZN3dsp39StaticDescriptorInitializer_dsp_2eprotoC1Ev:
000098d4	e92d4080	stmdb	sp!, {r7, lr}
000098d8	e28d7000	add	r7, sp, #0	; 0x0
000098dc	e24dd004	sub	sp, sp, #4	; 0x4
000098e0	e58d0000	str	r0, [sp]
000098e4	ebffe6c1	bl	__ZN3dsp30protobuf_BuildDesc_dsp_2eprotoEv
000098e8	e247d000	sub	sp, r7, #0	; 0x0
000098ec	e8bd8080	ldmia	sp!, {r7, pc}
_dsp_free:
000098f0	e92d4080	stmdb	sp!, {r7, lr}
000098f4	e28d7000	add	r7, sp, #0	; 0x0
000098f8	e24dd004	sub	sp, sp, #4	; 0x4
000098fc	e58d0000	str	r0, [sp]
00009900	e59d3000	ldr	r3, [sp]
00009904	e5933004	ldr	r3, [r3, #4]
00009908	e1a00003	mov	r0, r3
0000990c	eb0006b4	bl	0xb3e4	; symbol stub for: _free
00009910	e59d3000	ldr	r3, [sp]
00009914	e5933000	ldr	r3, [r3]
00009918	e1a00003	mov	r0, r3
0000991c	eb0006b0	bl	0xb3e4	; symbol stub for: _free
00009920	e59d0000	ldr	r0, [sp]
00009924	eb0006ae	bl	0xb3e4	; symbol stub for: _free
00009928	e247d000	sub	sp, r7, #0	; 0x0
0000992c	e8bd8080	ldmia	sp!, {r7, pc}
_dsp_write_buffer:
00009930	e92d4080	stmdb	sp!, {r7, lr}
00009934	e28d7000	add	r7, sp, #0	; 0x0
00009938	e24dde12	sub	sp, sp, #288	; 0x120
0000993c	e58d0010	str	r0, [sp, #16]
00009940	e58d100c	str	r1, [sp, #12]
00009944	e58d2008	str	r2, [sp, #8]
00009948	e59d3010	ldr	r3, [sp, #16]
0000994c	e3530000	cmp	r3, #0	; 0x0
00009950	0a000082	beq	0x9b60
00009954	e59d3010	ldr	r3, [sp, #16]
00009958	e2833010	add	r3, r3, #16	; 0x10
0000995c	e1a00003	mov	r0, r3
00009960	eb0006b4	bl	0xb438	; symbol stub for: _pthread_mutex_lock
00009964	e3a00000	mov	r0, #0	; 0x0
00009968	eb0006c1	bl	0xb474	; symbol stub for: _time
0000996c	e1a03000	mov	r3, r0
00009970	e58d3118	str	r3, [sp, #280]
00009974	e28d3f46	add	r3, sp, #280	; 0x118
00009978	e1a00003	mov	r0, r3
0000997c	eb00069e	bl	0xb3fc	; symbol stub for: _localtime
00009980	e1a03000	mov	r3, r0
00009984	e58d311c	str	r3, [sp, #284]
00009988	ea000024	b	0x9a20
0000998c	e59d3010	ldr	r3, [sp, #16]
00009990	e5932004	ldr	r2, [r3, #4]
00009994	e28d3014	add	r3, sp, #20	; 0x14
00009998	e1a00003	mov	r0, r3
0000999c	e3a01c01	mov	r1, #256	; 0x100
000099a0	e59d311c	ldr	r3, [sp, #284]
000099a4	eb0006ac	bl	0xb45c	; symbol stub for: _strftime
000099a8	e59d3010	ldr	r3, [sp, #16]
000099ac	e5933008	ldr	r3, [r3, #8]
000099b0	e3530000	cmp	r3, #0	; 0x0
000099b4	0a000003	beq	0x99c8
000099b8	e59d3010	ldr	r3, [sp, #16]
000099bc	e5933008	ldr	r3, [r3, #8]
000099c0	e1a00003	mov	r0, r3
000099c4	eb00067a	bl	0xb3b4	; symbol stub for: _fclose
000099c8	e59f3198	ldr	r3, [pc, #408]	; 0x9b68
000099cc	e08f3003	add	r3, pc, r3
000099d0	e5933000	ldr	r3, [r3]
000099d4	e5933000	ldr	r3, [r3]
000099d8	e28d2014	add	r2, sp, #20	; 0x14
000099dc	e1a00003	mov	r0, r3
000099e0	e59f3184	ldr	r3, [pc, #388]	; 0x9b6c
000099e4	e08f3003	add	r3, pc, r3
000099e8	e1a01003	mov	r1, r3
000099ec	eb000679	bl	0xb3d8	; symbol stub for: _fprintf
000099f0	e59d2010	ldr	r2, [sp, #16]
000099f4	e3a03001	mov	r3, #1	; 0x1
000099f8	e582300c	str	r3, [r2, #12]
000099fc	e28d3014	add	r3, sp, #20	; 0x14
00009a00	e1a00003	mov	r0, r3
00009a04	e59f3164	ldr	r3, [pc, #356]	; 0x9b70
00009a08	e08f3003	add	r3, pc, r3
00009a0c	e1a01003	mov	r1, r3
00009a10	eb00066d	bl	0xb3cc	; symbol stub for: _fopen
00009a14	e1a02000	mov	r2, r0
00009a18	e59d3010	ldr	r3, [sp, #16]
00009a1c	e5832008	str	r2, [r3, #8]
00009a20	e59d3010	ldr	r3, [sp, #16]
00009a24	e5933008	ldr	r3, [r3, #8]
00009a28	e3530000	cmp	r3, #0	; 0x0
00009a2c	0affffd6	beq	0x998c
00009a30	e59d311c	ldr	r3, [sp, #284]
00009a34	e5931004	ldr	r1, [r3, #4]
00009a38	e59f3134	ldr	r3, [pc, #308]	; 0x9b74
00009a3c	e0c32391	smull	r2, r3, r1, r3
00009a40	e0833001	add	r3, r3, r1
00009a44	e1a02243	mov	r2, r3, asr #4
00009a48	e1a03fc1	mov	r3, r1, asr #31
00009a4c	e0632002	rsb	r2, r3, r2
00009a50	e58d2000	str	r2, [sp]
00009a54	e59d3000	ldr	r3, [sp]
00009a58	e1a02083	mov	r2, r3, lsl #1
00009a5c	e1a03202	mov	r3, r2, lsl #4
00009a60	e0623003	rsb	r3, r2, r3
00009a64	e0631001	rsb	r1, r3, r1
00009a68	e58d1000	str	r1, [sp]
00009a6c	e59d3000	ldr	r3, [sp]
00009a70	e3530000	cmp	r3, #0	; 0x0
00009a74	1a000003	bne	0x9a88
00009a78	e59d3010	ldr	r3, [sp, #16]
00009a7c	e593300c	ldr	r3, [r3, #12]
00009a80	e3530000	cmp	r3, #0	; 0x0
00009a84	0affffc0	beq	0x998c
00009a88	e59d311c	ldr	r3, [sp, #284]
00009a8c	e5931004	ldr	r1, [r3, #4]
00009a90	e59f30dc	ldr	r3, [pc, #220]	; 0x9b74
00009a94	e0c32391	smull	r2, r3, r1, r3
00009a98	e0833001	add	r3, r3, r1
00009a9c	e1a02243	mov	r2, r3, asr #4
00009aa0	e1a03fc1	mov	r3, r1, asr #31
00009aa4	e0632002	rsb	r2, r3, r2
00009aa8	e58d2004	str	r2, [sp, #4]
00009aac	e59d3004	ldr	r3, [sp, #4]
00009ab0	e1a02083	mov	r2, r3, lsl #1
00009ab4	e1a03202	mov	r3, r2, lsl #4
00009ab8	e0623003	rsb	r3, r2, r3
00009abc	e0631001	rsb	r1, r3, r1
00009ac0	e58d1004	str	r1, [sp, #4]
00009ac4	e59d3004	ldr	r3, [sp, #4]
00009ac8	e3530000	cmp	r3, #0	; 0x0
00009acc	0a000002	beq	0x9adc
00009ad0	e59d2010	ldr	r2, [sp, #16]
00009ad4	e3a03000	mov	r3, #0	; 0x0
00009ad8	e582300c	str	r3, [r2, #12]
00009adc	e59d3008	ldr	r3, [sp, #8]
00009ae0	e5933000	ldr	r3, [r3]
00009ae4	e2833024	add	r3, r3, #36	; 0x24
00009ae8	e5933000	ldr	r3, [r3]
00009aec	e59d0008	ldr	r0, [sp, #8]
00009af0	e12fff33	blx	r3
00009af4	e1a03000	mov	r3, r0
00009af8	e1a02403	mov	r2, r3, lsl #8
00009afc	e59d300c	ldr	r3, [sp, #12]
00009b00	e20330ff	and	r3, r3, #255	; 0xff
00009b04	e1823003	orr	r3, r2, r3
00009b08	e58d3114	str	r3, [sp, #276]
00009b0c	e59d3010	ldr	r3, [sp, #16]
00009b10	e593c008	ldr	ip, [r3, #8]
00009b14	e28d3f45	add	r3, sp, #276	; 0x114
00009b18	e1a00003	mov	r0, r3
00009b1c	e3a01001	mov	r1, #1	; 0x1
00009b20	e3a02004	mov	r2, #4	; 0x4
00009b24	e1a0300c	mov	r3, ip
00009b28	eb000630	bl	0xb3f0	; symbol stub for: _fwrite
00009b2c	e59d3010	ldr	r3, [sp, #16]
00009b30	e5933008	ldr	r3, [r3, #8]
00009b34	e59d0008	ldr	r0, [sp, #8]
00009b38	e1a01003	mov	r1, r3
00009b3c	eb0005f2	bl	0xb30c	; symbol stub for: __ZNK6google8protobuf7Message25SerializeToFileDescriptorEi
00009b40	e59d3010	ldr	r3, [sp, #16]
00009b44	e5933008	ldr	r3, [r3, #8]
00009b48	e1a00003	mov	r0, r3
00009b4c	eb00061b	bl	0xb3c0	; symbol stub for: _fflush
00009b50	e59d3010	ldr	r3, [sp, #16]
00009b54	e2833010	add	r3, r3, #16	; 0x10
00009b58	e1a00003	mov	r0, r3
00009b5c	eb000638	bl	0xb444	; symbol stub for: _pthread_mutex_unlock
00009b60	e247d000	sub	sp, r7, #0	; 0x0
00009b64	e8bd8080	ldmia	sp!, {r7, pc}
00009b68	00002638	andeq	r2, r0, r8, lsr r6
00009b6c	00002290	muleq	r0, r0, r2
00009b70	00002278	andeq	r2, r0, r8, ror r2
00009b74	88888889	stmhiia	r8, {r0, r3, r7, r11, pc}
_dsp_add_microphone_sample:
00009b78	e92d40f0	stmdb	sp!, {r4, r5, r6, r7, lr}
00009b7c	e28d700c	add	r7, sp, #12	; 0xc
00009b80	e92d0d00	stmdb	sp!, {r8, r10, r11}
00009b84	ed2d8b11	fstmdbx	sp!, {d8-d15}
00009b88	e24dd064	sub	sp, sp, #100	; 0x64
00009b8c	e58d0044	str	r0, [sp, #68]
00009b90	e58d1040	str	r1, [sp, #64]
00009b94	e58d203c	str	r2, [sp, #60]
00009b98	e59f3128	ldr	r3, [pc, #296]	; 0x9cc8
00009b9c	e08f3003	add	r3, pc, r3
00009ba0	e5933000	ldr	r3, [r3]
00009ba4	e58d3020	str	r3, [sp, #32]
00009ba8	e59f311c	ldr	r3, [pc, #284]	; 0x9ccc
00009bac	e08f3003	add	r3, pc, r3
00009bb0	e58d3024	str	r3, [sp, #36]
00009bb4	e28d2028	add	r2, sp, #40	; 0x28
00009bb8	e5827000	str	r7, [r2]
00009bbc	e59f310c	ldr	r3, [pc, #268]	; 0x9cd0
00009bc0	e08f3003	add	r3, pc, r3
00009bc4	e5823004	str	r3, [r2, #4]
00009bc8	e582d008	str	sp, [r2, #8]
00009bcc	e28d3008	add	r3, sp, #8	; 0x8
00009bd0	e1a00003	mov	r0, r3
00009bd4	eb0005a5	bl	0xb270	; symbol stub for: __Unwind_SjLj_Register
00009bd8	e28d2048	add	r2, sp, #72	; 0x48
00009bdc	e3e03000	mvn	r3, #0	; 0x0
00009be0	e58d300c	str	r3, [sp, #12]
00009be4	e1a00002	mov	r0, r2
00009be8	ebffe700	bl	__ZN3dsp17MicrophoneMessageC1Ev
00009bec	e3a03001	mov	r3, #1	; 0x1
00009bf0	e58d300c	str	r3, [sp, #12]
00009bf4	eb000603	bl	0xb408	; symbol stub for: _mach_absolute_time
00009bf8	e1a02000	mov	r2, r0
00009bfc	e1a03001	mov	r3, r1
00009c00	e1a01002	mov	r1, r2
00009c04	e1a02003	mov	r2, r3
00009c08	e28d3048	add	r3, sp, #72	; 0x48
00009c0c	e1a00003	mov	r0, r3
00009c10	e59f30bc	ldr	r3, [pc, #188]	; 0x9cd4
00009c14	e08f3003	add	r3, pc, r3
00009c18	e5933000	ldr	r3, [r3]
00009c1c	e12fff33	blx	r3
00009c20	e28d3048	add	r3, sp, #72	; 0x48
00009c24	e1a00003	mov	r0, r3
00009c28	e59d1040	ldr	r1, [sp, #64]
00009c2c	e59d203c	ldr	r2, [sp, #60]
00009c30	e59f30a0	ldr	r3, [pc, #160]	; 0x9cd8
00009c34	e08f3003	add	r3, pc, r3
00009c38	e5933000	ldr	r3, [r3]
00009c3c	e12fff33	blx	r3
00009c40	e28d3048	add	r3, sp, #72	; 0x48
00009c44	e59d0044	ldr	r0, [sp, #68]
00009c48	e3a01005	mov	r1, #5	; 0x5
00009c4c	e1a02003	mov	r2, r3
00009c50	ebffff36	bl	_dsp_write_buffer
00009c54	ea00000e	b	0x9c94
00009c58	e59d3010	ldr	r3, [sp, #16]
00009c5c	e58d3000	str	r3, [sp]
00009c60	e59d3000	ldr	r3, [sp]
00009c64	e58d3004	str	r3, [sp, #4]
00009c68	e28d2048	add	r2, sp, #72	; 0x48
00009c6c	e3a03000	mov	r3, #0	; 0x0
00009c70	e58d300c	str	r3, [sp, #12]
00009c74	e1a00002	mov	r0, r2
00009c78	ebfff7e1	bl	__ZN3dsp17MicrophoneMessageD1Ev
00009c7c	e59d3004	ldr	r3, [sp, #4]
00009c80	e58d3000	str	r3, [sp]
00009c84	e3e03000	mvn	r3, #0	; 0x0
00009c88	e58d300c	str	r3, [sp, #12]
00009c8c	e59d0000	ldr	r0, [sp]
00009c90	eb000579	bl	0xb27c	; symbol stub for: __Unwind_SjLj_Resume
00009c94	e28d3048	add	r3, sp, #72	; 0x48
00009c98	e3e02000	mvn	r2, #0	; 0x0
00009c9c	e58d200c	str	r2, [sp, #12]
00009ca0	e1a00003	mov	r0, r3
00009ca4	ebfff7d6	bl	__ZN3dsp17MicrophoneMessageD1Ev
00009ca8	e28d3008	add	r3, sp, #8	; 0x8
00009cac	e1a00003	mov	r0, r3
00009cb0	eb000574	bl	0xb288	; symbol stub for: __Unwind_SjLj_Unregister
00009cb4	e247d05c	sub	sp, r7, #92	; 0x5c
00009cb8	ecbd8b11	fldmiax	sp!, {d8-d15}
00009cbc	e247d018	sub	sp, r7, #24	; 0x18
00009cc0	e8bd0d00	ldmia	sp!, {r8, r10, r11}
00009cc4	e8bd80f0	ldmia	sp!, {r4, r5, r6, r7, pc}
00009cc8	0000247c	andeq	r2, r0, ip, ror r4
00009ccc	00002b00	andeq	r2, r0, r0, lsl #22
00009cd0	00000090	muleq	r0, r0, r0
00009cd4	0000242c	andeq	r2, r0, ip, lsr #8
00009cd8	00002460	andeq	r2, r0, r0, ror #8
00009cdc	e1a00000	nop			(mov r0,r0)
_dsp_set_microphone_description:
00009ce0	e92d40f0	stmdb	sp!, {r4, r5, r6, r7, lr}
00009ce4	e28d700c	add	r7, sp, #12	; 0xc
00009ce8	e92d0d00	stmdb	sp!, {r8, r10, r11}
00009cec	ed2d8b11	fstmdbx	sp!, {d8-d15}
00009cf0	e24dd068	sub	sp, sp, #104	; 0x68
00009cf4	e58d0048	str	r0, [sp, #72]
00009cf8	e58d1044	str	r1, [sp, #68]
00009cfc	e58d2040	str	r2, [sp, #64]
00009d00	e58d303c	str	r3, [sp, #60]
00009d04	e59f30f4	ldr	r3, [pc, #244]	; 0x9e00
00009d08	e08f3003	add	r3, pc, r3
00009d0c	e5933000	ldr	r3, [r3]
00009d10	e58d3020	str	r3, [sp, #32]
00009d14	e59f30e8	ldr	r3, [pc, #232]	; 0x9e04
00009d18	e08f3003	add	r3, pc, r3
00009d1c	e58d3024	str	r3, [sp, #36]
00009d20	e28d2028	add	r2, sp, #40	; 0x28
00009d24	e5827000	str	r7, [r2]
00009d28	e59f30d8	ldr	r3, [pc, #216]	; 0x9e08
00009d2c	e08f3003	add	r3, pc, r3
00009d30	e5823004	str	r3, [r2, #4]
00009d34	e582d008	str	sp, [r2, #8]
00009d38	e28d3008	add	r3, sp, #8	; 0x8
00009d3c	e1a00003	mov	r0, r3
00009d40	eb00054a	bl	0xb270	; symbol stub for: __Unwind_SjLj_Register
00009d44	e28d204c	add	r2, sp, #76	; 0x4c
00009d48	e3e03000	mvn	r3, #0	; 0x0
00009d4c	e58d300c	str	r3, [sp, #12]
00009d50	e1a00002	mov	r0, r2
00009d54	ebffe6fa	bl	__ZN3dsp21MicrophoneDescriptionC1Ev
00009d58	e28d304c	add	r3, sp, #76	; 0x4c
00009d5c	e1a00003	mov	r0, r3
00009d60	e59d1044	ldr	r1, [sp, #68]
00009d64	e59f30a0	ldr	r3, [pc, #160]	; 0x9e0c
00009d68	e08f3003	add	r3, pc, r3
00009d6c	e5933000	ldr	r3, [r3]
00009d70	e12fff33	blx	r3
00009d74	e28d204c	add	r2, sp, #76	; 0x4c
00009d78	e3a03001	mov	r3, #1	; 0x1
00009d7c	e58d300c	str	r3, [sp, #12]
00009d80	e59d0048	ldr	r0, [sp, #72]
00009d84	e3a01004	mov	r1, #4	; 0x4
00009d88	ebfffee8	bl	_dsp_write_buffer
00009d8c	ea00000e	b	0x9dcc
00009d90	e59d3010	ldr	r3, [sp, #16]
00009d94	e58d3000	str	r3, [sp]
00009d98	e59d3000	ldr	r3, [sp]
00009d9c	e58d3004	str	r3, [sp, #4]
00009da0	e28d204c	add	r2, sp, #76	; 0x4c
00009da4	e3a03000	mov	r3, #0	; 0x0
00009da8	e58d300c	str	r3, [sp, #12]
00009dac	e1a00002	mov	r0, r2
00009db0	ebfff4f8	bl	__ZN3dsp21MicrophoneDescriptionD1Ev
00009db4	e59d3004	ldr	r3, [sp, #4]
00009db8	e58d3000	str	r3, [sp]
00009dbc	e3e03000	mvn	r3, #0	; 0x0
00009dc0	e58d300c	str	r3, [sp, #12]
00009dc4	e59d0000	ldr	r0, [sp]
00009dc8	eb00052b	bl	0xb27c	; symbol stub for: __Unwind_SjLj_Resume
00009dcc	e28d304c	add	r3, sp, #76	; 0x4c
00009dd0	e3e02000	mvn	r2, #0	; 0x0
00009dd4	e58d200c	str	r2, [sp, #12]
00009dd8	e1a00003	mov	r0, r3
00009ddc	ebfff4ed	bl	__ZN3dsp21MicrophoneDescriptionD1Ev
00009de0	e28d3008	add	r3, sp, #8	; 0x8
00009de4	e1a00003	mov	r0, r3
00009de8	eb000526	bl	0xb288	; symbol stub for: __Unwind_SjLj_Unregister
00009dec	e247d05c	sub	sp, r7, #92	; 0x5c
00009df0	ecbd8b11	fldmiax	sp!, {d8-d15}
00009df4	e247d018	sub	sp, r7, #24	; 0x18
00009df8	e8bd0d00	ldmia	sp!, {r8, r10, r11}
00009dfc	e8bd80f0	ldmia	sp!, {r4, r5, r6, r7, pc}
00009e00	00002310	andeq	r2, r0, r0, lsl r3
00009e04	0000299a	muleq	r0, r10, r9
00009e08	0000005c	andeq	r0, r0, ip, asr r0
00009e0c	000022e8	andeq	r2, r0, r8, ror #5
00009e10	e1a00000	nop			(mov r0,r0)
_dsp_add_location_sample:
00009e14	e24dd004	sub	sp, sp, #4	; 0x4
00009e18	e92d40f0	stmdb	sp!, {r4, r5, r6, r7, lr}
00009e1c	e28d700c	add	r7, sp, #12	; 0xc
00009e20	e92d0d00	stmdb	sp!, {r8, r10, r11}
00009e24	ed2d8b11	fstmdbx	sp!, {d8-d15}
00009e28	e24dd088	sub	sp, sp, #136	; 0x88
00009e2c	e58d0044	str	r0, [sp, #68]
00009e30	e58d103c	str	r1, [sp, #60]
00009e34	e58d2040	str	r2, [sp, #64]
00009e38	e58d30ec	str	r3, [sp, #236]
00009e3c	e59f31b0	ldr	r3, [pc, #432]	; 0x9ff4
00009e40	e08f3003	add	r3, pc, r3
00009e44	e5933000	ldr	r3, [r3]
00009e48	e58d3020	str	r3, [sp, #32]
00009e4c	e59f31a4	ldr	r3, [pc, #420]	; 0x9ff8
00009e50	e08f3003	add	r3, pc, r3
00009e54	e58d3024	str	r3, [sp, #36]
00009e58	e28d2028	add	r2, sp, #40	; 0x28
00009e5c	e5827000	str	r7, [r2]
00009e60	e59f3194	ldr	r3, [pc, #404]	; 0x9ffc
00009e64	e08f3003	add	r3, pc, r3
00009e68	e5823004	str	r3, [r2, #4]
00009e6c	e582d008	str	sp, [r2, #8]
00009e70	e28d3008	add	r3, sp, #8	; 0x8
00009e74	e1a00003	mov	r0, r3
00009e78	eb0004fc	bl	0xb270	; symbol stub for: __Unwind_SjLj_Register
00009e7c	e28d2048	add	r2, sp, #72	; 0x48
00009e80	e3e03000	mvn	r3, #0	; 0x0
00009e84	e58d300c	str	r3, [sp, #12]
00009e88	e1a00002	mov	r0, r2
00009e8c	ebffe59d	bl	__ZN3dsp15LocationMessageC1Ev
00009e90	e3a03001	mov	r3, #1	; 0x1
00009e94	e58d300c	str	r3, [sp, #12]
00009e98	eb00055a	bl	0xb408	; symbol stub for: _mach_absolute_time
00009e9c	e1a02000	mov	r2, r0
00009ea0	e1a03001	mov	r3, r1
00009ea4	e1a01002	mov	r1, r2
00009ea8	e1a02003	mov	r2, r3
00009eac	e28d3048	add	r3, sp, #72	; 0x48
00009eb0	e1a00003	mov	r0, r3
00009eb4	e59f3144	ldr	r3, [pc, #324]	; 0xa000
00009eb8	e08f3003	add	r3, pc, r3
00009ebc	e5933000	ldr	r3, [r3]
00009ec0	e12fff33	blx	r3
00009ec4	e28d3048	add	r3, sp, #72	; 0x48
00009ec8	e1a00003	mov	r0, r3
00009ecc	e28d103c	add	r1, sp, #60	; 0x3c
00009ed0	e8910006	ldmia	r1, {r1, r2}
00009ed4	e59f3128	ldr	r3, [pc, #296]	; 0xa004
00009ed8	e08f3003	add	r3, pc, r3
00009edc	e5933000	ldr	r3, [r3]
00009ee0	e12fff33	blx	r3
00009ee4	e28d3048	add	r3, sp, #72	; 0x48
00009ee8	e1a00003	mov	r0, r3
00009eec	e28d10ec	add	r1, sp, #236	; 0xec
00009ef0	e8910006	ldmia	r1, {r1, r2}
00009ef4	e59f310c	ldr	r3, [pc, #268]	; 0xa008
00009ef8	e08f3003	add	r3, pc, r3
00009efc	e5933000	ldr	r3, [r3]
00009f00	e12fff33	blx	r3
00009f04	e28d3048	add	r3, sp, #72	; 0x48
00009f08	e1a00003	mov	r0, r3
00009f0c	e28d10f4	add	r1, sp, #244	; 0xf4
00009f10	e8910006	ldmia	r1, {r1, r2}
00009f14	e59f30f0	ldr	r3, [pc, #240]	; 0xa00c
00009f18	e08f3003	add	r3, pc, r3
00009f1c	e5933000	ldr	r3, [r3]
00009f20	e12fff33	blx	r3
00009f24	e28d3048	add	r3, sp, #72	; 0x48
00009f28	e1a00003	mov	r0, r3
00009f2c	e28d10fc	add	r1, sp, #252	; 0xfc
00009f30	e8910006	ldmia	r1, {r1, r2}
00009f34	e59f30d4	ldr	r3, [pc, #212]	; 0xa010
00009f38	e08f3003	add	r3, pc, r3
00009f3c	e5933000	ldr	r3, [r3]
00009f40	e12fff33	blx	r3
00009f44	e28d3048	add	r3, sp, #72	; 0x48
00009f48	e1a00003	mov	r0, r3
00009f4c	e28d1f41	add	r1, sp, #260	; 0x104
00009f50	e8910006	ldmia	r1, {r1, r2}
00009f54	e59f30b8	ldr	r3, [pc, #184]	; 0xa014
00009f58	e08f3003	add	r3, pc, r3
00009f5c	e5933000	ldr	r3, [r3]
00009f60	e12fff33	blx	r3
00009f64	e28d3048	add	r3, sp, #72	; 0x48
00009f68	e59d0044	ldr	r0, [sp, #68]
00009f6c	e3a01007	mov	r1, #7	; 0x7
00009f70	e1a02003	mov	r2, r3
00009f74	ebfffe6d	bl	_dsp_write_buffer
00009f78	ea00000e	b	0x9fb8
00009f7c	e59d3010	ldr	r3, [sp, #16]
00009f80	e58d3000	str	r3, [sp]
00009f84	e59d3000	ldr	r3, [sp]
00009f88	e58d3004	str	r3, [sp, #4]
00009f8c	e28d2048	add	r2, sp, #72	; 0x48
00009f90	e3a03000	mov	r3, #0	; 0x0
00009f94	e58d300c	str	r3, [sp, #12]
00009f98	e1a00002	mov	r0, r2
00009f9c	ebfffc7e	bl	__ZN3dsp15LocationMessageD1Ev
00009fa0	e59d3004	ldr	r3, [sp, #4]
00009fa4	e58d3000	str	r3, [sp]
00009fa8	e3e03000	mvn	r3, #0	; 0x0
00009fac	e58d300c	str	r3, [sp, #12]
00009fb0	e59d0000	ldr	r0, [sp]
00009fb4	eb0004b0	bl	0xb27c	; symbol stub for: __Unwind_SjLj_Resume
00009fb8	e28d3048	add	r3, sp, #72	; 0x48
00009fbc	e3e02000	mvn	r2, #0	; 0x0
00009fc0	e58d200c	str	r2, [sp, #12]
00009fc4	e1a00003	mov	r0, r3
00009fc8	ebfffc73	bl	__ZN3dsp15LocationMessageD1Ev
00009fcc	e28d3008	add	r3, sp, #8	; 0x8
00009fd0	e1a00003	mov	r0, r3
00009fd4	eb0004ab	bl	0xb288	; symbol stub for: __Unwind_SjLj_Unregister
00009fd8	e247d05c	sub	sp, r7, #92	; 0x5c
00009fdc	ecbd8b11	fldmiax	sp!, {d8-d15}
00009fe0	e247d018	sub	sp, r7, #24	; 0x18
00009fe4	e8bd0d00	ldmia	sp!, {r8, r10, r11}
00009fe8	e8bd40f0	ldmia	sp!, {r4, r5, r6, r7, lr}
00009fec	e28dd004	add	sp, sp, #4	; 0x4
00009ff0	e12fff1e	bx	lr
00009ff4	000021d8	ldreqd	r2, [r0], -r8
00009ff8	00002868	andeq	r2, r0, r8, ror #16
00009ffc	00000110	andeq	r0, r0, r0, lsl r1
0000a000	000021a8	andeq	r2, r0, r8, lsr #3
0000a004	00002190	muleq	r0, r0, r1
0000a008	00002190	muleq	r0, r0, r1
0000a00c	00002134	andeq	r2, r0, r4, lsr r1
0000a010	0000210c	andeq	r2, r0, ip, lsl #2
0000a014	00002104	andeq	r2, r0, r4, lsl #2
0000a018	e1a00000	nop			(mov r0,r0)
_dsp_set_location_description:
0000a01c	e92d40f0	stmdb	sp!, {r4, r5, r6, r7, lr}
0000a020	e28d700c	add	r7, sp, #12	; 0xc
0000a024	e92d0d00	stmdb	sp!, {r8, r10, r11}
0000a028	ed2d8b11	fstmdbx	sp!, {d8-d15}
0000a02c	e24dd058	sub	sp, sp, #88	; 0x58
0000a030	e58d0040	str	r0, [sp, #64]
0000a034	e58d103c	str	r1, [sp, #60]
0000a038	e59f30f4	ldr	r3, [pc, #244]	; 0xa134
0000a03c	e08f3003	add	r3, pc, r3
0000a040	e5933000	ldr	r3, [r3]
0000a044	e58d3020	str	r3, [sp, #32]
0000a048	e59f30e8	ldr	r3, [pc, #232]	; 0xa138
0000a04c	e08f3003	add	r3, pc, r3
0000a050	e58d3024	str	r3, [sp, #36]
0000a054	e28d2028	add	r2, sp, #40	; 0x28
0000a058	e5827000	str	r7, [r2]
0000a05c	e59f30d8	ldr	r3, [pc, #216]	; 0xa13c
0000a060	e08f3003	add	r3, pc, r3
0000a064	e5823004	str	r3, [r2, #4]
0000a068	e582d008	str	sp, [r2, #8]
0000a06c	e28d3008	add	r3, sp, #8	; 0x8
0000a070	e1a00003	mov	r0, r3
0000a074	eb00047d	bl	0xb270	; symbol stub for: __Unwind_SjLj_Register
0000a078	e28d2044	add	r2, sp, #68	; 0x44
0000a07c	e3e03000	mvn	r3, #0	; 0x0
0000a080	e58d300c	str	r3, [sp, #12]
0000a084	e1a00002	mov	r0, r2
0000a088	ebffe589	bl	__ZN3dsp19LocationDescriptionC1Ev
0000a08c	e28d3044	add	r3, sp, #68	; 0x44
0000a090	e1a00003	mov	r0, r3
0000a094	e59d103c	ldr	r1, [sp, #60]
0000a098	e59f30a0	ldr	r3, [pc, #160]	; 0xa140
0000a09c	e08f3003	add	r3, pc, r3
0000a0a0	e5933000	ldr	r3, [r3]
0000a0a4	e12fff33	blx	r3
0000a0a8	e28d2044	add	r2, sp, #68	; 0x44
0000a0ac	e3a03001	mov	r3, #1	; 0x1
0000a0b0	e58d300c	str	r3, [sp, #12]
0000a0b4	e59d0040	ldr	r0, [sp, #64]
0000a0b8	e3a01006	mov	r1, #6	; 0x6
0000a0bc	ebfffe1b	bl	_dsp_write_buffer
0000a0c0	ea00000e	b	0xa100
0000a0c4	e59d3010	ldr	r3, [sp, #16]
0000a0c8	e58d3000	str	r3, [sp]
0000a0cc	e59d3000	ldr	r3, [sp]
0000a0d0	e58d3004	str	r3, [sp, #4]
0000a0d4	e28d2044	add	r2, sp, #68	; 0x44
0000a0d8	e3a03000	mov	r3, #0	; 0x0
0000a0dc	e58d300c	str	r3, [sp, #12]
0000a0e0	e1a00002	mov	r0, r2
0000a0e4	ebfff971	bl	__ZN3dsp19LocationDescriptionD1Ev
0000a0e8	e59d3004	ldr	r3, [sp, #4]
0000a0ec	e58d3000	str	r3, [sp]
0000a0f0	e3e03000	mvn	r3, #0	; 0x0
0000a0f4	e58d300c	str	r3, [sp, #12]
0000a0f8	e59d0000	ldr	r0, [sp]
0000a0fc	eb00045e	bl	0xb27c	; symbol stub for: __Unwind_SjLj_Resume
0000a100	e28d3044	add	r3, sp, #68	; 0x44
0000a104	e3e02000	mvn	r2, #0	; 0x0
0000a108	e58d200c	str	r2, [sp, #12]
0000a10c	e1a00003	mov	r0, r3
0000a110	ebfff966	bl	__ZN3dsp19LocationDescriptionD1Ev
0000a114	e28d3008	add	r3, sp, #8	; 0x8
0000a118	e1a00003	mov	r0, r3
0000a11c	eb000459	bl	0xb288	; symbol stub for: __Unwind_SjLj_Unregister
0000a120	e247d05c	sub	sp, r7, #92	; 0x5c
0000a124	ecbd8b11	fldmiax	sp!, {d8-d15}
0000a128	e247d018	sub	sp, r7, #24	; 0x18
0000a12c	e8bd0d00	ldmia	sp!, {r8, r10, r11}
0000a130	e8bd80f0	ldmia	sp!, {r4, r5, r6, r7, pc}
0000a134	00001fdc	ldreqd	r1, [r0], -ip
0000a138	00002672	andeq	r2, r0, r2, ror r6
0000a13c	0000005c	andeq	r0, r0, ip, asr r0
0000a140	00001f9c	muleq	r0, ip, pc
0000a144	e1a00000	nop			(mov r0,r0)
_dsp_add_accelerometer_sample:
0000a148	e92d40f0	stmdb	sp!, {r4, r5, r6, r7, lr}
0000a14c	e28d700c	add	r7, sp, #12	; 0xc
0000a150	e92d0d00	stmdb	sp!, {r8, r10, r11}
0000a154	ed2d8b11	fstmdbx	sp!, {d8-d15}
0000a158	e24dd070	sub	sp, sp, #112	; 0x70
0000a15c	e58d0048	str	r0, [sp, #72]
0000a160	e58d1044	str	r1, [sp, #68]
0000a164	e58d2040	str	r2, [sp, #64]
0000a168	e58d303c	str	r3, [sp, #60]
0000a16c	e59f315c	ldr	r3, [pc, #348]	; 0xa2d0
0000a170	e08f3003	add	r3, pc, r3
0000a174	e5933000	ldr	r3, [r3]
0000a178	e58d3020	str	r3, [sp, #32]
0000a17c	e59f3150	ldr	r3, [pc, #336]	; 0xa2d4
0000a180	e08f3003	add	r3, pc, r3
0000a184	e58d3024	str	r3, [sp, #36]
0000a188	e28d2028	add	r2, sp, #40	; 0x28
0000a18c	e5827000	str	r7, [r2]
0000a190	e59f3140	ldr	r3, [pc, #320]	; 0xa2d8
0000a194	e08f3003	add	r3, pc, r3
0000a198	e5823004	str	r3, [r2, #4]
0000a19c	e582d008	str	sp, [r2, #8]
0000a1a0	e28d3008	add	r3, sp, #8	; 0x8
0000a1a4	e1a00003	mov	r0, r3
0000a1a8	eb000430	bl	0xb270	; symbol stub for: __Unwind_SjLj_Register
0000a1ac	e28d204c	add	r2, sp, #76	; 0x4c
0000a1b0	e3e03000	mvn	r3, #0	; 0x0
0000a1b4	e58d300c	str	r3, [sp, #12]
0000a1b8	e1a00002	mov	r0, r2
0000a1bc	ebffe635	bl	__ZN3dsp20AccelerometerMessageC1Ev
0000a1c0	e3a03001	mov	r3, #1	; 0x1
0000a1c4	e58d300c	str	r3, [sp, #12]
0000a1c8	eb00048e	bl	0xb408	; symbol stub for: _mach_absolute_time
0000a1cc	e1a02000	mov	r2, r0
0000a1d0	e1a03001	mov	r3, r1
0000a1d4	e1a01002	mov	r1, r2
0000a1d8	e1a02003	mov	r2, r3
0000a1dc	e28d304c	add	r3, sp, #76	; 0x4c
0000a1e0	e1a00003	mov	r0, r3
0000a1e4	e59f30f0	ldr	r3, [pc, #240]	; 0xa2dc
0000a1e8	e08f3003	add	r3, pc, r3
0000a1ec	e5933000	ldr	r3, [r3]
0000a1f0	e12fff33	blx	r3
0000a1f4	e28d304c	add	r3, sp, #76	; 0x4c
0000a1f8	e1a00003	mov	r0, r3
0000a1fc	e59d1044	ldr	r1, [sp, #68]
0000a200	e59f30d8	ldr	r3, [pc, #216]	; 0xa2e0
0000a204	e08f3003	add	r3, pc, r3
0000a208	e5933000	ldr	r3, [r3]
0000a20c	e12fff33	blx	r3
0000a210	e28d304c	add	r3, sp, #76	; 0x4c
0000a214	e1a00003	mov	r0, r3
0000a218	e59d1040	ldr	r1, [sp, #64]
0000a21c	e59f30c0	ldr	r3, [pc, #192]	; 0xa2e4
0000a220	e08f3003	add	r3, pc, r3
0000a224	e5933000	ldr	r3, [r3]
0000a228	e12fff33	blx	r3
0000a22c	e28d304c	add	r3, sp, #76	; 0x4c
0000a230	e1a00003	mov	r0, r3
0000a234	e59d103c	ldr	r1, [sp, #60]
0000a238	e59f30a8	ldr	r3, [pc, #168]	; 0xa2e8
0000a23c	e08f3003	add	r3, pc, r3
0000a240	e5933000	ldr	r3, [r3]
0000a244	e12fff33	blx	r3
0000a248	e28d304c	add	r3, sp, #76	; 0x4c
0000a24c	e59d0048	ldr	r0, [sp, #72]
0000a250	e3a01003	mov	r1, #3	; 0x3
0000a254	e1a02003	mov	r2, r3
0000a258	ebfffdb4	bl	_dsp_write_buffer
0000a25c	ea00000e	b	0xa29c
0000a260	e59d3010	ldr	r3, [sp, #16]
0000a264	e58d3000	str	r3, [sp]
0000a268	e59d3000	ldr	r3, [sp]
0000a26c	e58d3004	str	r3, [sp, #4]
0000a270	e28d204c	add	r2, sp, #76	; 0x4c
0000a274	e3a03000	mov	r3, #0	; 0x0
0000a278	e58d300c	str	r3, [sp, #12]
0000a27c	e1a00002	mov	r0, r2
0000a280	ebfff14b	bl	__ZN3dsp20AccelerometerMessageD1Ev
0000a284	e59d3004	ldr	r3, [sp, #4]
0000a288	e58d3000	str	r3, [sp]
0000a28c	e3e03000	mvn	r3, #0	; 0x0
0000a290	e58d300c	str	r3, [sp, #12]
0000a294	e59d0000	ldr	r0, [sp]
0000a298	eb0003f7	bl	0xb27c	; symbol stub for: __Unwind_SjLj_Resume
0000a29c	e28d304c	add	r3, sp, #76	; 0x4c
0000a2a0	e3e02000	mvn	r2, #0	; 0x0
0000a2a4	e58d200c	str	r2, [sp, #12]
0000a2a8	e1a00003	mov	r0, r3
0000a2ac	ebfff140	bl	__ZN3dsp20AccelerometerMessageD1Ev
0000a2b0	e28d3008	add	r3, sp, #8	; 0x8
0000a2b4	e1a00003	mov	r0, r3
0000a2b8	eb0003f2	bl	0xb288	; symbol stub for: __Unwind_SjLj_Unregister
0000a2bc	e247d05c	sub	sp, r7, #92	; 0x5c
0000a2c0	ecbd8b11	fldmiax	sp!, {d8-d15}
0000a2c4	e247d018	sub	sp, r7, #24	; 0x18
0000a2c8	e8bd0d00	ldmia	sp!, {r8, r10, r11}
0000a2cc	e8bd80f0	ldmia	sp!, {r4, r5, r6, r7, pc}
0000a2d0	00001ea8	andeq	r1, r0, r8, lsr #29
0000a2d4	00002544	andeq	r2, r0, r4, asr #10
0000a2d8	000000c4	andeq	r0, r0, r4, asr #1
0000a2dc	00001eb0	streqh	r1, [r0], -r0
0000a2e0	00001e74	andeq	r1, r0, r4, ror lr
0000a2e4	00001e60	andeq	r1, r0, r0, ror #28
0000a2e8	00001df4	streqd	r1, [r0], -r4
0000a2ec	e1a00000	nop			(mov r0,r0)
_dsp_set_accelerometer_description:
0000a2f0	e92d40f0	stmdb	sp!, {r4, r5, r6, r7, lr}
0000a2f4	e28d700c	add	r7, sp, #12	; 0xc
0000a2f8	e92d0d00	stmdb	sp!, {r8, r10, r11}
0000a2fc	ed2d8b11	fstmdbx	sp!, {d8-d15}
0000a300	e24dd058	sub	sp, sp, #88	; 0x58
0000a304	e58d0040	str	r0, [sp, #64]
0000a308	e58d103c	str	r1, [sp, #60]
0000a30c	e59f30f4	ldr	r3, [pc, #244]	; 0xa408
0000a310	e08f3003	add	r3, pc, r3
0000a314	e5933000	ldr	r3, [r3]
0000a318	e58d3020	str	r3, [sp, #32]
0000a31c	e59f30e8	ldr	r3, [pc, #232]	; 0xa40c
0000a320	e08f3003	add	r3, pc, r3
0000a324	e58d3024	str	r3, [sp, #36]
0000a328	e28d2028	add	r2, sp, #40	; 0x28
0000a32c	e5827000	str	r7, [r2]
0000a330	e59f30d8	ldr	r3, [pc, #216]	; 0xa410
0000a334	e08f3003	add	r3, pc, r3
0000a338	e5823004	str	r3, [r2, #4]
0000a33c	e582d008	str	sp, [r2, #8]
0000a340	e28d3008	add	r3, sp, #8	; 0x8
0000a344	e1a00003	mov	r0, r3
0000a348	eb0003c8	bl	0xb270	; symbol stub for: __Unwind_SjLj_Register
0000a34c	e28d2044	add	r2, sp, #68	; 0x44
0000a350	e3e03000	mvn	r3, #0	; 0x0
0000a354	e58d300c	str	r3, [sp, #12]
0000a358	e1a00002	mov	r0, r2
0000a35c	ebffe627	bl	__ZN3dsp24AccelerometerDescriptionC1Ev
0000a360	e28d3044	add	r3, sp, #68	; 0x44
0000a364	e1a00003	mov	r0, r3
0000a368	e59d103c	ldr	r1, [sp, #60]
0000a36c	e59f30a0	ldr	r3, [pc, #160]	; 0xa414
0000a370	e08f3003	add	r3, pc, r3
0000a374	e5933000	ldr	r3, [r3]
0000a378	e12fff33	blx	r3
0000a37c	e28d2044	add	r2, sp, #68	; 0x44
0000a380	e3a03001	mov	r3, #1	; 0x1
0000a384	e58d300c	str	r3, [sp, #12]
0000a388	e59d0040	ldr	r0, [sp, #64]
0000a38c	e3a01002	mov	r1, #2	; 0x2
0000a390	ebfffd66	bl	_dsp_write_buffer
0000a394	ea00000e	b	0xa3d4
0000a398	e59d3010	ldr	r3, [sp, #16]
0000a39c	e58d3000	str	r3, [sp]
0000a3a0	e59d3000	ldr	r3, [sp]
0000a3a4	e58d3004	str	r3, [sp, #4]
0000a3a8	e28d2044	add	r2, sp, #68	; 0x44
0000a3ac	e3a03000	mov	r3, #0	; 0x0
0000a3b0	e58d300c	str	r3, [sp, #12]
0000a3b4	e1a00002	mov	r0, r2
0000a3b8	ebffee75	bl	__ZN3dsp24AccelerometerDescriptionD1Ev
0000a3bc	e59d3004	ldr	r3, [sp, #4]
0000a3c0	e58d3000	str	r3, [sp]
0000a3c4	e3e03000	mvn	r3, #0	; 0x0
0000a3c8	e58d300c	str	r3, [sp, #12]
0000a3cc	e59d0000	ldr	r0, [sp]
0000a3d0	eb0003a9	bl	0xb27c	; symbol stub for: __Unwind_SjLj_Resume
0000a3d4	e28d3044	add	r3, sp, #68	; 0x44
0000a3d8	e3e02000	mvn	r2, #0	; 0x0
0000a3dc	e58d200c	str	r2, [sp, #12]
0000a3e0	e1a00003	mov	r0, r3
0000a3e4	ebffee6a	bl	__ZN3dsp24AccelerometerDescriptionD1Ev
0000a3e8	e28d3008	add	r3, sp, #8	; 0x8
0000a3ec	e1a00003	mov	r0, r3
0000a3f0	eb0003a4	bl	0xb288	; symbol stub for: __Unwind_SjLj_Unregister
0000a3f4	e247d05c	sub	sp, r7, #92	; 0x5c
0000a3f8	ecbd8b11	fldmiax	sp!, {d8-d15}
0000a3fc	e247d018	sub	sp, r7, #24	; 0x18
0000a400	e8bd0d00	ldmia	sp!, {r8, r10, r11}
0000a404	e8bd80f0	ldmia	sp!, {r4, r5, r6, r7, pc}
0000a408	00001d08	andeq	r1, r0, r8, lsl #26
0000a40c	000023aa	andeq	r2, r0, r10, lsr #7
0000a410	0000005c	andeq	r0, r0, ip, asr r0
0000a414	00001d1c	andeq	r1, r0, ip, lsl sp
0000a418	e1a00000	nop			(mov r0,r0)
_dsp_init:
0000a41c	e92d40f0	stmdb	sp!, {r4, r5, r6, r7, lr}
0000a420	e28d700c	add	r7, sp, #12	; 0xc
0000a424	e92d0d00	stmdb	sp!, {r8, r10, r11}
0000a428	ed2d8b11	fstmdbx	sp!, {d8-d15}
0000a42c	e24dd07c	sub	sp, sp, #124	; 0x7c
0000a430	e58d0048	str	r0, [sp, #72]
0000a434	e58d1044	str	r1, [sp, #68]
0000a438	e59f3254	ldr	r3, [pc, #596]	; 0xa694
0000a43c	e08f3003	add	r3, pc, r3
0000a440	e5933000	ldr	r3, [r3]
0000a444	e58d3028	str	r3, [sp, #40]
0000a448	e59f3248	ldr	r3, [pc, #584]	; 0xa698
0000a44c	e08f3003	add	r3, pc, r3
0000a450	e58d302c	str	r3, [sp, #44]
0000a454	e28d2030	add	r2, sp, #48	; 0x30
0000a458	e5827000	str	r7, [r2]
0000a45c	e59f3238	ldr	r3, [pc, #568]	; 0xa69c
0000a460	e08f3003	add	r3, pc, r3
0000a464	e5823004	str	r3, [r2, #4]
0000a468	e582d008	str	sp, [r2, #8]
0000a46c	e28d3010	add	r3, sp, #16	; 0x10
0000a470	e1a00003	mov	r0, r3
0000a474	eb00037d	bl	0xb270	; symbol stub for: __Unwind_SjLj_Register
0000a478	e3a0003c	mov	r0, #60	; 0x3c
0000a47c	eb0003e7	bl	0xb420	; symbol stub for: _malloc
0000a480	e1a03000	mov	r3, r0
0000a484	e58d3078	str	r3, [sp, #120]
0000a488	e59d0048	ldr	r0, [sp, #72]
0000a48c	eb0003f5	bl	0xb468	; symbol stub for: _strlen
0000a490	e1a03000	mov	r3, r0
0000a494	e2833001	add	r3, r3, #1	; 0x1
0000a498	e1a00003	mov	r0, r3
0000a49c	eb0003df	bl	0xb420	; symbol stub for: _malloc
0000a4a0	e1a03000	mov	r3, r0
0000a4a4	e1a02003	mov	r2, r3
0000a4a8	e59d3078	ldr	r3, [sp, #120]
0000a4ac	e5832004	str	r2, [r3, #4]
0000a4b0	e59d0044	ldr	r0, [sp, #68]
0000a4b4	eb0003eb	bl	0xb468	; symbol stub for: _strlen
0000a4b8	e1a03000	mov	r3, r0
0000a4bc	e2833001	add	r3, r3, #1	; 0x1
0000a4c0	e1a00003	mov	r0, r3
0000a4c4	eb0003d5	bl	0xb420	; symbol stub for: _malloc
0000a4c8	e1a03000	mov	r3, r0
0000a4cc	e1a02003	mov	r2, r3
0000a4d0	e59d3078	ldr	r3, [sp, #120]
0000a4d4	e5832000	str	r2, [r3]
0000a4d8	e59d2078	ldr	r2, [sp, #120]
0000a4dc	e3a03000	mov	r3, #0	; 0x0
0000a4e0	e5823008	str	r3, [r2, #8]
0000a4e4	e59d2078	ldr	r2, [sp, #120]
0000a4e8	e3a03000	mov	r3, #0	; 0x0
0000a4ec	e582300c	str	r3, [r2, #12]
0000a4f0	e59d3078	ldr	r3, [sp, #120]
0000a4f4	e5933004	ldr	r3, [r3, #4]
0000a4f8	e1a00003	mov	r0, r3
0000a4fc	e59d1048	ldr	r1, [sp, #72]
0000a500	eb0003d2	bl	0xb450	; symbol stub for: _strcpy
0000a504	e59d3078	ldr	r3, [sp, #120]
0000a508	e5933000	ldr	r3, [r3]
0000a50c	e1a00003	mov	r0, r3
0000a510	e59d1044	ldr	r1, [sp, #68]
0000a514	eb0003cd	bl	0xb450	; symbol stub for: _strcpy
0000a518	e59d3078	ldr	r3, [sp, #120]
0000a51c	e2832010	add	r2, r3, #16	; 0x10
0000a520	e3e03000	mvn	r3, #0	; 0x0
0000a524	e58d3014	str	r3, [sp, #20]
0000a528	e1a00002	mov	r0, r2
0000a52c	e3a01000	mov	r1, #0	; 0x0
0000a530	eb0003bd	bl	0xb42c	; symbol stub for: _pthread_mutex_init
0000a534	e28d304c	add	r3, sp, #76	; 0x4c
0000a538	e1a00003	mov	r0, r3
0000a53c	ebffe5fe	bl	__ZN3dsp6HeaderC1Ev
0000a540	e28d2070	add	r2, sp, #112	; 0x70
0000a544	e3a03001	mov	r3, #1	; 0x1
0000a548	e58d3014	str	r3, [sp, #20]
0000a54c	e1a00002	mov	r0, r2
0000a550	eb0003af	bl	0xb414	; symbol stub for: _mach_timebase_info
0000a554	e28d304c	add	r3, sp, #76	; 0x4c
0000a558	e1a00003	mov	r0, r3
0000a55c	e59d1044	ldr	r1, [sp, #68]
0000a560	e59f3138	ldr	r3, [pc, #312]	; 0xa6a0
0000a564	e08f3003	add	r3, pc, r3
0000a568	e5933000	ldr	r3, [r3]
0000a56c	e12fff33	blx	r3
0000a570	e3a00000	mov	r0, #0	; 0x0
0000a574	eb0003be	bl	0xb474	; symbol stub for: _time
0000a578	e1a03000	mov	r3, r0
0000a57c	e1a02003	mov	r2, r3
0000a580	e28d304c	add	r3, sp, #76	; 0x4c
0000a584	e1a00003	mov	r0, r3
0000a588	e1a01002	mov	r1, r2
0000a58c	e59f3110	ldr	r3, [pc, #272]	; 0xa6a4
0000a590	e08f3003	add	r3, pc, r3
0000a594	e5933000	ldr	r3, [r3]
0000a598	e12fff33	blx	r3
0000a59c	e59d3070	ldr	r3, [sp, #112]
0000a5a0	e59d2074	ldr	r2, [sp, #116]
0000a5a4	e1a00003	mov	r0, r3
0000a5a8	e1a01002	mov	r1, r2
0000a5ac	eb00036b	bl	0xb360	; symbol stub for: ___udivsi3
0000a5b0	e1a03000	mov	r3, r0
0000a5b4	e1a02003	mov	r2, r3
0000a5b8	e28d304c	add	r3, sp, #76	; 0x4c
0000a5bc	e1a00003	mov	r0, r3
0000a5c0	e1a01002	mov	r1, r2
0000a5c4	e59f30dc	ldr	r3, [pc, #220]	; 0xa6a8
0000a5c8	e08f3003	add	r3, pc, r3
0000a5cc	e5933000	ldr	r3, [r3]
0000a5d0	e12fff33	blx	r3
0000a5d4	eb00038b	bl	0xb408	; symbol stub for: _mach_absolute_time
0000a5d8	e1a02001	mov	r2, r1
0000a5dc	e1a01000	mov	r1, r0
0000a5e0	e28d304c	add	r3, sp, #76	; 0x4c
0000a5e4	e1a00003	mov	r0, r3
0000a5e8	e59f30bc	ldr	r3, [pc, #188]	; 0xa6ac
0000a5ec	e08f3003	add	r3, pc, r3
0000a5f0	e5933000	ldr	r3, [r3]
0000a5f4	e12fff33	blx	r3
0000a5f8	e28d304c	add	r3, sp, #76	; 0x4c
0000a5fc	e59d0078	ldr	r0, [sp, #120]
0000a600	e3a01001	mov	r1, #1	; 0x1
0000a604	e1a02003	mov	r2, r3
0000a608	ebfffcc8	bl	_dsp_write_buffer
0000a60c	e59d3078	ldr	r3, [sp, #120]
0000a610	e58d3008	str	r3, [sp, #8]
0000a614	e28d204c	add	r2, sp, #76	; 0x4c
0000a618	e3e03000	mvn	r3, #0	; 0x0
0000a61c	e58d3014	str	r3, [sp, #20]
0000a620	e1a00002	mov	r0, r2
0000a624	ebffeb2f	bl	__ZN3dsp6HeaderD1Ev
0000a628	e59d3008	ldr	r3, [sp, #8]
0000a62c	e58d3004	str	r3, [sp, #4]
0000a630	ea00000e	b	0xa670
0000a634	e59d3018	ldr	r3, [sp, #24]
0000a638	e58d3000	str	r3, [sp]
0000a63c	e59d3000	ldr	r3, [sp]
0000a640	e58d300c	str	r3, [sp, #12]
0000a644	e28d204c	add	r2, sp, #76	; 0x4c
0000a648	e3a03000	mov	r3, #0	; 0x0
0000a64c	e58d3014	str	r3, [sp, #20]
0000a650	e1a00002	mov	r0, r2
0000a654	ebffeb23	bl	__ZN3dsp6HeaderD1Ev
0000a658	e59d300c	ldr	r3, [sp, #12]
0000a65c	e58d3000	str	r3, [sp]
0000a660	e3e03000	mvn	r3, #0	; 0x0
0000a664	e58d3014	str	r3, [sp, #20]
0000a668	e59d0000	ldr	r0, [sp]
0000a66c	eb000302	bl	0xb27c	; symbol stub for: __Unwind_SjLj_Resume
0000a670	e28d3010	add	r3, sp, #16	; 0x10
0000a674	e1a00003	mov	r0, r3
0000a678	eb000302	bl	0xb288	; symbol stub for: __Unwind_SjLj_Unregister
0000a67c	e59d0004	ldr	r0, [sp, #4]
0000a680	e247d05c	sub	sp, r7, #92	; 0x5c
0000a684	ecbd8b11	fldmiax	sp!, {d8-d15}
0000a688	e247d018	sub	sp, r7, #24	; 0x18
0000a68c	e8bd0d00	ldmia	sp!, {r8, r10, r11}
0000a690	e8bd80f0	ldmia	sp!, {r4, r5, r6, r7, pc}
0000a694	00001bdc	ldreqd	r1, [r0], -ip
0000a698	00002284	andeq	r2, r0, r4, lsl #5
0000a69c	000001cc	andeq	r0, r0, ip, asr #3
0000a6a0	00001ad0	ldreqd	r1, [r0], -r0
0000a6a4	00001ac8	andeq	r1, r0, r8, asr #21
0000a6a8	00001aa4	andeq	r1, r0, r4, lsr #21
0000a6ac	00001a88	andeq	r1, r0, r8, lsl #21
__ZN3dsp6Header8_set_bitEi:
0000a6b0	e92d4080	stmdb	sp!, {r7, lr}
0000a6b4	e28d7000	add	r7, sp, #0	; 0x0
0000a6b8	e24dd008	sub	sp, sp, #8	; 0x8
0000a6bc	e58d0004	str	r0, [sp, #4]
0000a6c0	e58d1000	str	r1, [sp]
0000a6c4	e59d3000	ldr	r3, [sp]
0000a6c8	e283201f	add	r2, r3, #31	; 0x1f
0000a6cc	e3530000	cmp	r3, #0	; 0x0
0000a6d0	b1a03002	movlt	r3, r2
0000a6d4	e1a032c3	mov	r3, r3, asr #5
0000a6d8	e1a0c003	mov	ip, r3
0000a6dc	e59d2004	ldr	r2, [sp, #4]
0000a6e0	e1a03103	mov	r3, r3, lsl #2
0000a6e4	e0833002	add	r3, r3, r2
0000a6e8	e5930020	ldr	r0, [r3, #32]
0000a6ec	e59d2000	ldr	r2, [sp]
0000a6f0	e1a03fc2	mov	r3, r2, asr #31
0000a6f4	e1a01da3	mov	r1, r3, lsr #27
0000a6f8	e0823001	add	r3, r2, r1
0000a6fc	e203301f	and	r3, r3, #31	; 0x1f
0000a700	e0613003	rsb	r3, r1, r3
0000a704	e1a02003	mov	r2, r3
0000a708	e3a03001	mov	r3, #1	; 0x1
0000a70c	e1a03213	mov	r3, r3, lsl r2
0000a710	e1801003	orr	r1, r0, r3
0000a714	e59d2004	ldr	r2, [sp, #4]
0000a718	e1a0310c	mov	r3, ip, lsl #2
0000a71c	e0833002	add	r3, r3, r2
0000a720	e5831020	str	r1, [r3, #32]
0000a724	e247d000	sub	sp, r7, #0	; 0x0
0000a728	e8bd8080	ldmia	sp!, {r7, pc}
__ZN3dsp24AccelerometerDescription8_set_bitEi:
0000a72c	e92d4080	stmdb	sp!, {r7, lr}
0000a730	e28d7000	add	r7, sp, #0	; 0x0
0000a734	e24dd008	sub	sp, sp, #8	; 0x8
0000a738	e58d0004	str	r0, [sp, #4]
0000a73c	e58d1000	str	r1, [sp]
0000a740	e59d3000	ldr	r3, [sp]
0000a744	e283201f	add	r2, r3, #31	; 0x1f
0000a748	e3530000	cmp	r3, #0	; 0x0
0000a74c	b1a03002	movlt	r3, r2
0000a750	e1a032c3	mov	r3, r3, asr #5
0000a754	e1a0c003	mov	ip, r3
0000a758	e59d2004	ldr	r2, [sp, #4]
0000a75c	e1a03103	mov	r3, r3, lsl #2
0000a760	e0833002	add	r3, r3, r2
0000a764	e5930010	ldr	r0, [r3, #16]
0000a768	e59d2000	ldr	r2, [sp]
0000a76c	e1a03fc2	mov	r3, r2, asr #31
0000a770	e1a01da3	mov	r1, r3, lsr #27
0000a774	e0823001	add	r3, r2, r1
0000a778	e203301f	and	r3, r3, #31	; 0x1f
0000a77c	e0613003	rsb	r3, r1, r3
0000a780	e1a02003	mov	r2, r3
0000a784	e3a03001	mov	r3, #1	; 0x1
0000a788	e1a03213	mov	r3, r3, lsl r2
0000a78c	e1801003	orr	r1, r0, r3
0000a790	e59d2004	ldr	r2, [sp, #4]
0000a794	e1a0310c	mov	r3, ip, lsl #2
0000a798	e0833002	add	r3, r3, r2
0000a79c	e5831010	str	r1, [r3, #16]
0000a7a0	e247d000	sub	sp, r7, #0	; 0x0
0000a7a4	e8bd8080	ldmia	sp!, {r7, pc}
__ZN3dsp20AccelerometerMessage8_set_bitEi:
0000a7a8	e92d4080	stmdb	sp!, {r7, lr}
0000a7ac	e28d7000	add	r7, sp, #0	; 0x0
0000a7b0	e24dd008	sub	sp, sp, #8	; 0x8
0000a7b4	e58d0004	str	r0, [sp, #4]
0000a7b8	e58d1000	str	r1, [sp]
0000a7bc	e59d3000	ldr	r3, [sp]
0000a7c0	e283201f	add	r2, r3, #31	; 0x1f
0000a7c4	e3530000	cmp	r3, #0	; 0x0
0000a7c8	b1a03002	movlt	r3, r2
0000a7cc	e1a032c3	mov	r3, r3, asr #5
0000a7d0	e1a0c003	mov	ip, r3
0000a7d4	e59d2004	ldr	r2, [sp, #4]
0000a7d8	e1a03103	mov	r3, r3, lsl #2
0000a7dc	e0833002	add	r3, r3, r2
0000a7e0	e5930020	ldr	r0, [r3, #32]
0000a7e4	e59d2000	ldr	r2, [sp]
0000a7e8	e1a03fc2	mov	r3, r2, asr #31
0000a7ec	e1a01da3	mov	r1, r3, lsr #27
0000a7f0	e0823001	add	r3, r2, r1
0000a7f4	e203301f	and	r3, r3, #31	; 0x1f
0000a7f8	e0613003	rsb	r3, r1, r3
0000a7fc	e1a02003	mov	r2, r3
0000a800	e3a03001	mov	r3, #1	; 0x1
0000a804	e1a03213	mov	r3, r3, lsl r2
0000a808	e1801003	orr	r1, r0, r3
0000a80c	e59d2004	ldr	r2, [sp, #4]
0000a810	e1a0310c	mov	r3, ip, lsl #2
0000a814	e0833002	add	r3, r3, r2
0000a818	e5831020	str	r1, [r3, #32]
0000a81c	e247d000	sub	sp, r7, #0	; 0x0
0000a820	e8bd8080	ldmia	sp!, {r7, pc}
__ZN3dsp21MicrophoneDescription8_set_bitEi:
0000a824	e92d4080	stmdb	sp!, {r7, lr}
0000a828	e28d7000	add	r7, sp, #0	; 0x0
0000a82c	e24dd008	sub	sp, sp, #8	; 0x8
0000a830	e58d0004	str	r0, [sp, #4]
0000a834	e58d1000	str	r1, [sp]
0000a838	e59d3000	ldr	r3, [sp]
0000a83c	e283201f	add	r2, r3, #31	; 0x1f
0000a840	e3530000	cmp	r3, #0	; 0x0
0000a844	b1a03002	movlt	r3, r2
0000a848	e1a032c3	mov	r3, r3, asr #5
0000a84c	e1a0c003	mov	ip, r3
0000a850	e59d2004	ldr	r2, [sp, #4]
0000a854	e1a03103	mov	r3, r3, lsl #2
0000a858	e0833002	add	r3, r3, r2
0000a85c	e5930018	ldr	r0, [r3, #24]
0000a860	e59d2000	ldr	r2, [sp]
0000a864	e1a03fc2	mov	r3, r2, asr #31
0000a868	e1a01da3	mov	r1, r3, lsr #27
0000a86c	e0823001	add	r3, r2, r1
0000a870	e203301f	and	r3, r3, #31	; 0x1f
0000a874	e0613003	rsb	r3, r1, r3
0000a878	e1a02003	mov	r2, r3
0000a87c	e3a03001	mov	r3, #1	; 0x1
0000a880	e1a03213	mov	r3, r3, lsl r2
0000a884	e1801003	orr	r1, r0, r3
0000a888	e59d2004	ldr	r2, [sp, #4]
0000a88c	e1a0310c	mov	r3, ip, lsl #2
0000a890	e0833002	add	r3, r3, r2
0000a894	e5831018	str	r1, [r3, #24]
0000a898	e247d000	sub	sp, r7, #0	; 0x0
0000a89c	e8bd8080	ldmia	sp!, {r7, pc}
__ZN3dsp17MicrophoneMessage8_set_bitEi:
0000a8a0	e92d4080	stmdb	sp!, {r7, lr}
0000a8a4	e28d7000	add	r7, sp, #0	; 0x0
0000a8a8	e24dd008	sub	sp, sp, #8	; 0x8
0000a8ac	e58d0004	str	r0, [sp, #4]
0000a8b0	e58d1000	str	r1, [sp]
0000a8b4	e59d3000	ldr	r3, [sp]
0000a8b8	e283201f	add	r2, r3, #31	; 0x1f
0000a8bc	e3530000	cmp	r3, #0	; 0x0
0000a8c0	b1a03002	movlt	r3, r2
0000a8c4	e1a032c3	mov	r3, r3, asr #5
0000a8c8	e1a0c003	mov	ip, r3
0000a8cc	e59d2004	ldr	r2, [sp, #4]
0000a8d0	e1a03103	mov	r3, r3, lsl #2
0000a8d4	e0833002	add	r3, r3, r2
0000a8d8	e5930018	ldr	r0, [r3, #24]
0000a8dc	e59d2000	ldr	r2, [sp]
0000a8e0	e1a03fc2	mov	r3, r2, asr #31
0000a8e4	e1a01da3	mov	r1, r3, lsr #27
0000a8e8	e0823001	add	r3, r2, r1
0000a8ec	e203301f	and	r3, r3, #31	; 0x1f
0000a8f0	e0613003	rsb	r3, r1, r3
0000a8f4	e1a02003	mov	r2, r3
0000a8f8	e3a03001	mov	r3, #1	; 0x1
0000a8fc	e1a03213	mov	r3, r3, lsl r2
0000a900	e1801003	orr	r1, r0, r3
0000a904	e59d2004	ldr	r2, [sp, #4]
0000a908	e1a0310c	mov	r3, ip, lsl #2
0000a90c	e0833002	add	r3, r3, r2
0000a910	e5831018	str	r1, [r3, #24]
0000a914	e247d000	sub	sp, r7, #0	; 0x0
0000a918	e8bd8080	ldmia	sp!, {r7, pc}
__ZN3dsp19LocationDescription8_set_bitEi:
0000a91c	e92d4080	stmdb	sp!, {r7, lr}
0000a920	e28d7000	add	r7, sp, #0	; 0x0
0000a924	e24dd008	sub	sp, sp, #8	; 0x8
0000a928	e58d0004	str	r0, [sp, #4]
0000a92c	e58d1000	str	r1, [sp]
0000a930	e59d3000	ldr	r3, [sp]
0000a934	e283201f	add	r2, r3, #31	; 0x1f
0000a938	e3530000	cmp	r3, #0	; 0x0
0000a93c	b1a03002	movlt	r3, r2
0000a940	e1a032c3	mov	r3, r3, asr #5
0000a944	e1a0c003	mov	ip, r3
0000a948	e59d2004	ldr	r2, [sp, #4]
0000a94c	e1a03103	mov	r3, r3, lsl #2
0000a950	e0833002	add	r3, r3, r2
0000a954	e5930010	ldr	r0, [r3, #16]
0000a958	e59d2000	ldr	r2, [sp]
0000a95c	e1a03fc2	mov	r3, r2, asr #31
0000a960	e1a01da3	mov	r1, r3, lsr #27
0000a964	e0823001	add	r3, r2, r1
0000a968	e203301f	and	r3, r3, #31	; 0x1f
0000a96c	e0613003	rsb	r3, r1, r3
0000a970	e1a02003	mov	r2, r3
0000a974	e3a03001	mov	r3, #1	; 0x1
0000a978	e1a03213	mov	r3, r3, lsl r2
0000a97c	e1801003	orr	r1, r0, r3
0000a980	e59d2004	ldr	r2, [sp, #4]
0000a984	e1a0310c	mov	r3, ip, lsl #2
0000a988	e0833002	add	r3, r3, r2
0000a98c	e5831010	str	r1, [r3, #16]
0000a990	e247d000	sub	sp, r7, #0	; 0x0
0000a994	e8bd8080	ldmia	sp!, {r7, pc}
__ZN3dsp15LocationMessage8_set_bitEi:
0000a998	e92d4080	stmdb	sp!, {r7, lr}
0000a99c	e28d7000	add	r7, sp, #0	; 0x0
0000a9a0	e24dd008	sub	sp, sp, #8	; 0x8
0000a9a4	e58d0004	str	r0, [sp, #4]
0000a9a8	e58d1000	str	r1, [sp]
0000a9ac	e59d3000	ldr	r3, [sp]
0000a9b0	e283201f	add	r2, r3, #31	; 0x1f
0000a9b4	e3530000	cmp	r3, #0	; 0x0
0000a9b8	b1a03002	movlt	r3, r2
0000a9bc	e1a032c3	mov	r3, r3, asr #5
0000a9c0	e1a0c003	mov	ip, r3
0000a9c4	e59d2004	ldr	r2, [sp, #4]
0000a9c8	e1a03103	mov	r3, r3, lsl #2
0000a9cc	e0833002	add	r3, r3, r2
0000a9d0	e593003c	ldr	r0, [r3, #60]
0000a9d4	e59d2000	ldr	r2, [sp]
0000a9d8	e1a03fc2	mov	r3, r2, asr #31
0000a9dc	e1a01da3	mov	r1, r3, lsr #27
0000a9e0	e0823001	add	r3, r2, r1
0000a9e4	e203301f	and	r3, r3, #31	; 0x1f
0000a9e8	e0613003	rsb	r3, r1, r3
0000a9ec	e1a02003	mov	r2, r3
0000a9f0	e3a03001	mov	r3, #1	; 0x1
0000a9f4	e1a03213	mov	r3, r3, lsl r2
0000a9f8	e1801003	orr	r1, r0, r3
0000a9fc	e59d2004	ldr	r2, [sp, #4]
0000aa00	e1a0310c	mov	r3, ip, lsl #2
0000aa04	e0833002	add	r3, r3, r2
0000aa08	e583103c	str	r1, [r3, #60]
0000aa0c	e247d000	sub	sp, r7, #0	; 0x0
0000aa10	e8bd8080	ldmia	sp!, {r7, pc}
__ZN3dsp6Header12set_sourceidEPKc:
0000aa14	e92d40f0	stmdb	sp!, {r4, r5, r6, r7, lr}
0000aa18	e28d700c	add	r7, sp, #12	; 0xc
0000aa1c	e92d0d00	stmdb	sp!, {r8, r10, r11}
0000aa20	ed2d8b11	fstmdbx	sp!, {d8-d15}
0000aa24	e24dd048	sub	sp, sp, #72	; 0x48
0000aa28	e58d0044	str	r0, [sp, #68]
0000aa2c	e58d1040	str	r1, [sp, #64]
0000aa30	e59f3110	ldr	r3, [pc, #272]	; 0xab48
0000aa34	e08f3003	add	r3, pc, r3
0000aa38	e5933000	ldr	r3, [r3]
0000aa3c	e58d3024	str	r3, [sp, #36]
0000aa40	e59f3104	ldr	r3, [pc, #260]	; 0xab4c
0000aa44	e08f3003	add	r3, pc, r3
0000aa48	e58d3028	str	r3, [sp, #40]
0000aa4c	e28d202c	add	r2, sp, #44	; 0x2c
0000aa50	e5827000	str	r7, [r2]
0000aa54	e59f30f4	ldr	r3, [pc, #244]	; 0xab50
0000aa58	e08f3003	add	r3, pc, r3
0000aa5c	e5823004	str	r3, [r2, #4]
0000aa60	e582d008	str	sp, [r2, #8]
0000aa64	e28d300c	add	r3, sp, #12	; 0xc
0000aa68	e1a00003	mov	r0, r3
0000aa6c	eb0001ff	bl	0xb270	; symbol stub for: __Unwind_SjLj_Register
0000aa70	e59d0044	ldr	r0, [sp, #68]
0000aa74	e3a01000	mov	r1, #0	; 0x0
0000aa78	e59f30d4	ldr	r3, [pc, #212]	; 0xab54
0000aa7c	e08f3003	add	r3, pc, r3
0000aa80	e5933000	ldr	r3, [r3]
0000aa84	e12fff33	blx	r3
0000aa88	e59d3044	ldr	r3, [sp, #68]
0000aa8c	e593200c	ldr	r2, [r3, #12]
0000aa90	e59f30c0	ldr	r3, [pc, #192]	; 0xab58
0000aa94	e08f3003	add	r3, pc, r3
0000aa98	e5933000	ldr	r3, [r3]
0000aa9c	e1520003	cmp	r2, r3
0000aaa0	1a000019	bne	0xab0c
0000aaa4	e3e03000	mvn	r3, #0	; 0x0
0000aaa8	e58d3010	str	r3, [sp, #16]
0000aaac	e3a00004	mov	r0, #4	; 0x4
0000aab0	eb000227	bl	0xb354	; symbol stub for: __Znwm
0000aab4	e1a03000	mov	r3, r0
0000aab8	e58d3004	str	r3, [sp, #4]
0000aabc	e3a03001	mov	r3, #1	; 0x1
0000aac0	e58d3010	str	r3, [sp, #16]
0000aac4	e59d0004	ldr	r0, [sp, #4]
0000aac8	eb000218	bl	0xb330	; symbol stub for: __ZNSsC1Ev
0000aacc	e59d3044	ldr	r3, [sp, #68]
0000aad0	e59d2004	ldr	r2, [sp, #4]
0000aad4	e583200c	str	r2, [r3, #12]
0000aad8	ea00000b	b	0xab0c
0000aadc	e59d3014	ldr	r3, [sp, #20]
0000aae0	e58d3000	str	r3, [sp]
0000aae4	e59d2000	ldr	r2, [sp]
0000aae8	e58d2008	str	r2, [sp, #8]
0000aaec	e59d0004	ldr	r0, [sp, #4]
0000aaf0	eb000214	bl	0xb348	; symbol stub for: __ZdlPv
0000aaf4	e59d3008	ldr	r3, [sp, #8]
0000aaf8	e58d3000	str	r3, [sp]
0000aafc	e3e03000	mvn	r3, #0	; 0x0
0000ab00	e58d3010	str	r3, [sp, #16]
0000ab04	e59d0000	ldr	r0, [sp]
0000ab08	eb0001db	bl	0xb27c	; symbol stub for: __Unwind_SjLj_Resume
0000ab0c	e59d3044	ldr	r3, [sp, #68]
0000ab10	e593200c	ldr	r2, [r3, #12]
0000ab14	e3e03000	mvn	r3, #0	; 0x0
0000ab18	e58d3010	str	r3, [sp, #16]
0000ab1c	e1a00002	mov	r0, r2
0000ab20	e59d1040	ldr	r1, [sp, #64]
0000ab24	eb0001fb	bl	0xb318	; symbol stub for: __ZNSs6assignEPKc
0000ab28	e28d300c	add	r3, sp, #12	; 0xc
0000ab2c	e1a00003	mov	r0, r3
0000ab30	eb0001d4	bl	0xb288	; symbol stub for: __Unwind_SjLj_Unregister
0000ab34	e247d05c	sub	sp, r7, #92	; 0x5c
0000ab38	ecbd8b11	fldmiax	sp!, {d8-d15}
0000ab3c	e247d018	sub	sp, r7, #24	; 0x18
0000ab40	e8bd0d00	ldmia	sp!, {r8, r10, r11}
0000ab44	e8bd80f0	ldmia	sp!, {r4, r5, r6, r7, pc}
0000ab48	000015e4	andeq	r1, r0, r4, ror #11
0000ab4c	00001c5c	andeq	r1, r0, ip, asr ip
0000ab50	0000007c	andeq	r0, r0, ip, ror r0
0000ab54	000015f4	streqd	r1, [r0], -r4
0000ab58	000015b4	streqh	r1, [r0], -r4
0000ab5c	e1a00000	nop			(mov r0,r0)
__ZN3dsp6Header17set_startunixtimeEj:
0000ab60	e92d4080	stmdb	sp!, {r7, lr}
0000ab64	e28d7000	add	r7, sp, #0	; 0x0
0000ab68	e24dd008	sub	sp, sp, #8	; 0x8
0000ab6c	e58d0004	str	r0, [sp, #4]
0000ab70	e58d1000	str	r1, [sp]
0000ab74	e59d0004	ldr	r0, [sp, #4]
0000ab78	e3a01001	mov	r1, #1	; 0x1
0000ab7c	e59f301c	ldr	r3, [pc, #28]	; 0xaba0
0000ab80	e08f3003	add	r3, pc, r3
0000ab84	e5933000	ldr	r3, [r3]
0000ab88	e12fff33	blx	r3
0000ab8c	e59d2004	ldr	r2, [sp, #4]
0000ab90	e59d3000	ldr	r3, [sp]
0000ab94	e5823010	str	r3, [r2, #16]
0000ab98	e247d000	sub	sp, r7, #0	; 0x0
0000ab9c	e8bd8080	ldmia	sp!, {r7, pc}
0000aba0	000014f0	streqd	r1, [r0], -r0
__ZN3dsp6Header21set_absolutetimeunitsEj:
0000aba4	e92d4080	stmdb	sp!, {r7, lr}
0000aba8	e28d7000	add	r7, sp, #0	; 0x0
0000abac	e24dd008	sub	sp, sp, #8	; 0x8
0000abb0	e58d0004	str	r0, [sp, #4]
0000abb4	e58d1000	str	r1, [sp]
0000abb8	e59d0004	ldr	r0, [sp, #4]
0000abbc	e3a01002	mov	r1, #2	; 0x2
0000abc0	e59f301c	ldr	r3, [pc, #28]	; 0xabe4
0000abc4	e08f3003	add	r3, pc, r3
0000abc8	e5933000	ldr	r3, [r3]
0000abcc	e12fff33	blx	r3
0000abd0	e59d2004	ldr	r2, [sp, #4]
0000abd4	e59d3000	ldr	r3, [sp]
0000abd8	e5823014	str	r3, [r2, #20]
0000abdc	e247d000	sub	sp, r7, #0	; 0x0
0000abe0	e8bd8080	ldmia	sp!, {r7, pc}
0000abe4	000014ac	andeq	r1, r0, ip, lsr #9
__ZN3dsp6Header21set_startabsolutetimeEy:
0000abe8	e92d4080	stmdb	sp!, {r7, lr}
0000abec	e28d7000	add	r7, sp, #0	; 0x0
0000abf0	e24dd00c	sub	sp, sp, #12	; 0xc
0000abf4	e58d0008	str	r0, [sp, #8]
0000abf8	e88d0006	stmia	sp, {r1, r2}
0000abfc	e59d0008	ldr	r0, [sp, #8]
0000ac00	e3a01003	mov	r1, #3	; 0x3
0000ac04	e59f3020	ldr	r3, [pc, #32]	; 0xac2c
0000ac08	e08f3003	add	r3, pc, r3
0000ac0c	e5933000	ldr	r3, [r3]
0000ac10	e12fff33	blx	r3
0000ac14	e59d1008	ldr	r1, [sp, #8]
0000ac18	e89d000c	ldmia	sp, {r2, r3}
0000ac1c	e5812018	str	r2, [r1, #24]
0000ac20	e581301c	str	r3, [r1, #28]
0000ac24	e247d000	sub	sp, r7, #0	; 0x0
0000ac28	e8bd8080	ldmia	sp!, {r7, pc}
0000ac2c	00001468	andeq	r1, r0, r8, ror #8
__ZN3dsp24AccelerometerDescription16set_samplingrateEf:
0000ac30	e92d4080	stmdb	sp!, {r7, lr}
0000ac34	e28d7000	add	r7, sp, #0	; 0x0
0000ac38	e24dd008	sub	sp, sp, #8	; 0x8
0000ac3c	e58d0004	str	r0, [sp, #4]
0000ac40	e58d1000	str	r1, [sp]
0000ac44	e59d0004	ldr	r0, [sp, #4]
0000ac48	e3a01000	mov	r1, #0	; 0x0
0000ac4c	e59f301c	ldr	r3, [pc, #28]	; 0xac70
0000ac50	e08f3003	add	r3, pc, r3
0000ac54	e5933000	ldr	r3, [r3]
0000ac58	e12fff33	blx	r3
0000ac5c	e59d2004	ldr	r2, [sp, #4]
0000ac60	e59d3000	ldr	r3, [sp]
0000ac64	e582300c	str	r3, [r2, #12]
0000ac68	e247d000	sub	sp, r7, #0	; 0x0
0000ac6c	e8bd8080	ldmia	sp!, {r7, pc}
0000ac70	00001440	andeq	r1, r0, r0, asr #8
__ZN3dsp20AccelerometerMessage13set_timestampEx:
0000ac74	e92d4080	stmdb	sp!, {r7, lr}
0000ac78	e28d7000	add	r7, sp, #0	; 0x0
0000ac7c	e24dd00c	sub	sp, sp, #12	; 0xc
0000ac80	e58d0008	str	r0, [sp, #8]
0000ac84	e88d0006	stmia	sp, {r1, r2}
0000ac88	e59d0008	ldr	r0, [sp, #8]
0000ac8c	e3a01000	mov	r1, #0	; 0x0
0000ac90	e59f3020	ldr	r3, [pc, #32]	; 0xacb8
0000ac94	e08f3003	add	r3, pc, r3
0000ac98	e5933000	ldr	r3, [r3]
0000ac9c	e12fff33	blx	r3
0000aca0	e59d1008	ldr	r1, [sp, #8]
0000aca4	e89d000c	ldmia	sp, {r2, r3}
0000aca8	e581200c	str	r2, [r1, #12]
0000acac	e5813010	str	r3, [r1, #16]
0000acb0	e247d000	sub	sp, r7, #0	; 0x0
0000acb4	e8bd8080	ldmia	sp!, {r7, pc}
0000acb8	000013c0	andeq	r1, r0, r0, asr #7
__ZN3dsp20AccelerometerMessage5set_xEf:
0000acbc	e92d4080	stmdb	sp!, {r7, lr}
0000acc0	e28d7000	add	r7, sp, #0	; 0x0
0000acc4	e24dd008	sub	sp, sp, #8	; 0x8
0000acc8	e58d0004	str	r0, [sp, #4]
0000accc	e58d1000	str	r1, [sp]
0000acd0	e59d0004	ldr	r0, [sp, #4]
0000acd4	e3a01001	mov	r1, #1	; 0x1
0000acd8	e59f301c	ldr	r3, [pc, #28]	; 0xacfc
0000acdc	e08f3003	add	r3, pc, r3
0000ace0	e5933000	ldr	r3, [r3]
0000ace4	e12fff33	blx	r3
0000ace8	e59d2004	ldr	r2, [sp, #4]
0000acec	e59d3000	ldr	r3, [sp]
0000acf0	e5823014	str	r3, [r2, #20]
0000acf4	e247d000	sub	sp, r7, #0	; 0x0
0000acf8	e8bd8080	ldmia	sp!, {r7, pc}
0000acfc	00001378	andeq	r1, r0, r8, ror r3
__ZN3dsp20AccelerometerMessage5set_yEf:
0000ad00	e92d4080	stmdb	sp!, {r7, lr}
0000ad04	e28d7000	add	r7, sp, #0	; 0x0
0000ad08	e24dd008	sub	sp, sp, #8	; 0x8
0000ad0c	e58d0004	str	r0, [sp, #4]
0000ad10	e58d1000	str	r1, [sp]
0000ad14	e59d0004	ldr	r0, [sp, #4]
0000ad18	e3a01002	mov	r1, #2	; 0x2
0000ad1c	e59f301c	ldr	r3, [pc, #28]	; 0xad40
0000ad20	e08f3003	add	r3, pc, r3
0000ad24	e5933000	ldr	r3, [r3]
0000ad28	e12fff33	blx	r3
0000ad2c	e59d2004	ldr	r2, [sp, #4]
0000ad30	e59d3000	ldr	r3, [sp]
0000ad34	e5823018	str	r3, [r2, #24]
0000ad38	e247d000	sub	sp, r7, #0	; 0x0
0000ad3c	e8bd8080	ldmia	sp!, {r7, pc}
0000ad40	00001334	andeq	r1, r0, r4, lsr r3
__ZN3dsp20AccelerometerMessage5set_zEf:
0000ad44	e92d4080	stmdb	sp!, {r7, lr}
0000ad48	e28d7000	add	r7, sp, #0	; 0x0
0000ad4c	e24dd008	sub	sp, sp, #8	; 0x8
0000ad50	e58d0004	str	r0, [sp, #4]
0000ad54	e58d1000	str	r1, [sp]
0000ad58	e59d0004	ldr	r0, [sp, #4]
0000ad5c	e3a01003	mov	r1, #3	; 0x3
0000ad60	e59f301c	ldr	r3, [pc, #28]	; 0xad84
0000ad64	e08f3003	add	r3, pc, r3
0000ad68	e5933000	ldr	r3, [r3]
0000ad6c	e12fff33	blx	r3
0000ad70	e59d2004	ldr	r2, [sp, #4]
0000ad74	e59d3000	ldr	r3, [sp]
0000ad78	e582301c	str	r3, [r2, #28]
0000ad7c	e247d000	sub	sp, r7, #0	; 0x0
0000ad80	e8bd8080	ldmia	sp!, {r7, pc}
0000ad84	000012f0	streqd	r1, [r0], -r0
__ZN3dsp21MicrophoneDescription16set_samplingrateEf:
0000ad88	e92d4080	stmdb	sp!, {r7, lr}
0000ad8c	e28d7000	add	r7, sp, #0	; 0x0
0000ad90	e24dd008	sub	sp, sp, #8	; 0x8
0000ad94	e58d0004	str	r0, [sp, #4]
0000ad98	e58d1000	str	r1, [sp]
0000ad9c	e59d0004	ldr	r0, [sp, #4]
0000ada0	e3a01000	mov	r1, #0	; 0x0
0000ada4	e59f301c	ldr	r3, [pc, #28]	; 0xadc8
0000ada8	e08f3003	add	r3, pc, r3
0000adac	e5933000	ldr	r3, [r3]
0000adb0	e12fff33	blx	r3
0000adb4	e59d2004	ldr	r2, [sp, #4]
0000adb8	e59d3000	ldr	r3, [sp]
0000adbc	e582300c	str	r3, [r2, #12]
0000adc0	e247d000	sub	sp, r7, #0	; 0x0
0000adc4	e8bd8080	ldmia	sp!, {r7, pc}
0000adc8	000012d4	ldreqd	r1, [r0], -r4
__ZN3dsp17MicrophoneMessage13set_timestampEx:
0000adcc	e92d4080	stmdb	sp!, {r7, lr}
0000add0	e28d7000	add	r7, sp, #0	; 0x0
0000add4	e24dd00c	sub	sp, sp, #12	; 0xc
0000add8	e58d0008	str	r0, [sp, #8]
0000addc	e88d0006	stmia	sp, {r1, r2}
0000ade0	e59d0008	ldr	r0, [sp, #8]
0000ade4	e3a01000	mov	r1, #0	; 0x0
0000ade8	e59f3020	ldr	r3, [pc, #32]	; 0xae10
0000adec	e08f3003	add	r3, pc, r3
0000adf0	e5933000	ldr	r3, [r3]
0000adf4	e12fff33	blx	r3
0000adf8	e59d1008	ldr	r1, [sp, #8]
0000adfc	e89d000c	ldmia	sp, {r2, r3}
0000ae00	e581200c	str	r2, [r1, #12]
0000ae04	e5813010	str	r3, [r1, #16]
0000ae08	e247d000	sub	sp, r7, #0	; 0x0
0000ae0c	e8bd8080	ldmia	sp!, {r7, pc}
0000ae10	000012b0	streqh	r1, [r0], -r0
__ZN3dsp17MicrophoneMessage11set_samplesEPKvm:
0000ae14	e92d40f0	stmdb	sp!, {r4, r5, r6, r7, lr}
0000ae18	e28d700c	add	r7, sp, #12	; 0xc
0000ae1c	e92d0d00	stmdb	sp!, {r8, r10, r11}
0000ae20	ed2d8b11	fstmdbx	sp!, {d8-d15}
0000ae24	e24dd04c	sub	sp, sp, #76	; 0x4c
0000ae28	e58d0048	str	r0, [sp, #72]
0000ae2c	e58d1044	str	r1, [sp, #68]
0000ae30	e58d2040	str	r2, [sp, #64]
0000ae34	e59f3114	ldr	r3, [pc, #276]	; 0xaf50
0000ae38	e08f3003	add	r3, pc, r3
0000ae3c	e5933000	ldr	r3, [r3]
0000ae40	e58d3024	str	r3, [sp, #36]
0000ae44	e59f3108	ldr	r3, [pc, #264]	; 0xaf54
0000ae48	e08f3003	add	r3, pc, r3
0000ae4c	e58d3028	str	r3, [sp, #40]
0000ae50	e28d202c	add	r2, sp, #44	; 0x2c
0000ae54	e5827000	str	r7, [r2]
0000ae58	e59f30f8	ldr	r3, [pc, #248]	; 0xaf58
0000ae5c	e08f3003	add	r3, pc, r3
0000ae60	e5823004	str	r3, [r2, #4]
0000ae64	e582d008	str	sp, [r2, #8]
0000ae68	e28d300c	add	r3, sp, #12	; 0xc
0000ae6c	e1a00003	mov	r0, r3
0000ae70	eb0000fe	bl	0xb270	; symbol stub for: __Unwind_SjLj_Register
0000ae74	e59d0048	ldr	r0, [sp, #72]
0000ae78	e3a01001	mov	r1, #1	; 0x1
0000ae7c	e59f30d8	ldr	r3, [pc, #216]	; 0xaf5c
0000ae80	e08f3003	add	r3, pc, r3
0000ae84	e5933000	ldr	r3, [r3]
0000ae88	e12fff33	blx	r3
0000ae8c	e59d3048	ldr	r3, [sp, #72]
0000ae90	e5932014	ldr	r2, [r3, #20]
0000ae94	e59f30c4	ldr	r3, [pc, #196]	; 0xaf60
0000ae98	e08f3003	add	r3, pc, r3
0000ae9c	e5933000	ldr	r3, [r3]
0000aea0	e1520003	cmp	r2, r3
0000aea4	1a000019	bne	0xaf10
0000aea8	e3e03000	mvn	r3, #0	; 0x0
0000aeac	e58d3010	str	r3, [sp, #16]
0000aeb0	e3a00004	mov	r0, #4	; 0x4
0000aeb4	eb000126	bl	0xb354	; symbol stub for: __Znwm
0000aeb8	e1a03000	mov	r3, r0
0000aebc	e58d3004	str	r3, [sp, #4]
0000aec0	e3a03001	mov	r3, #1	; 0x1
0000aec4	e58d3010	str	r3, [sp, #16]
0000aec8	e59d0004	ldr	r0, [sp, #4]
0000aecc	eb000117	bl	0xb330	; symbol stub for: __ZNSsC1Ev
0000aed0	e59d3048	ldr	r3, [sp, #72]
0000aed4	e59d2004	ldr	r2, [sp, #4]
0000aed8	e5832014	str	r2, [r3, #20]
0000aedc	ea00000b	b	0xaf10
0000aee0	e59d3014	ldr	r3, [sp, #20]
0000aee4	e58d3000	str	r3, [sp]
0000aee8	e59d2000	ldr	r2, [sp]
0000aeec	e58d2008	str	r2, [sp, #8]
0000aef0	e59d0004	ldr	r0, [sp, #4]
0000aef4	eb000113	bl	0xb348	; symbol stub for: __ZdlPv
0000aef8	e59d3008	ldr	r3, [sp, #8]
0000aefc	e58d3000	str	r3, [sp]
0000af00	e3e03000	mvn	r3, #0	; 0x0
0000af04	e58d3010	str	r3, [sp, #16]
0000af08	e59d0000	ldr	r0, [sp]
0000af0c	eb0000da	bl	0xb27c	; symbol stub for: __Unwind_SjLj_Resume
0000af10	e59d3048	ldr	r3, [sp, #72]
0000af14	e5932014	ldr	r2, [r3, #20]
0000af18	e59d1044	ldr	r1, [sp, #68]
0000af1c	e3e03000	mvn	r3, #0	; 0x0
0000af20	e58d3010	str	r3, [sp, #16]
0000af24	e1a00002	mov	r0, r2
0000af28	e59d2040	ldr	r2, [sp, #64]
0000af2c	eb0000fc	bl	0xb324	; symbol stub for: __ZNSs6assignEPKcm
0000af30	e28d300c	add	r3, sp, #12	; 0xc
0000af34	e1a00003	mov	r0, r3
0000af38	eb0000d2	bl	0xb288	; symbol stub for: __Unwind_SjLj_Unregister
0000af3c	e247d05c	sub	sp, r7, #92	; 0x5c
0000af40	ecbd8b11	fldmiax	sp!, {d8-d15}
0000af44	e247d018	sub	sp, r7, #24	; 0x18
0000af48	e8bd0d00	ldmia	sp!, {r8, r10, r11}
0000af4c	e8bd80f0	ldmia	sp!, {r4, r5, r6, r7, pc}
0000af50	000011e0	andeq	r1, r0, r0, ror #3
0000af54	0000185e	andeq	r1, r0, lr, asr r8
0000af58	0000007c	andeq	r0, r0, ip, ror r0
0000af5c	0000121c	andeq	r1, r0, ip, lsl r2
0000af60	000011a4	andeq	r1, r0, r4, lsr #3
0000af64	e1a00000	nop			(mov r0,r0)
__ZN3dsp19LocationDescription16set_samplingrateEf:
0000af68	e92d4080	stmdb	sp!, {r7, lr}
0000af6c	e28d7000	add	r7, sp, #0	; 0x0
0000af70	e24dd008	sub	sp, sp, #8	; 0x8
0000af74	e58d0004	str	r0, [sp, #4]
0000af78	e58d1000	str	r1, [sp]
0000af7c	e59d0004	ldr	r0, [sp, #4]
0000af80	e3a01000	mov	r1, #0	; 0x0
0000af84	e59f301c	ldr	r3, [pc, #28]	; 0xafa8
0000af88	e08f3003	add	r3, pc, r3
0000af8c	e5933000	ldr	r3, [r3]
0000af90	e12fff33	blx	r3
0000af94	e59d2004	ldr	r2, [sp, #4]
0000af98	e59d3000	ldr	r3, [sp]
0000af9c	e582300c	str	r3, [r2, #12]
0000afa0	e247d000	sub	sp, r7, #0	; 0x0
0000afa4	e8bd8080	ldmia	sp!, {r7, pc}
0000afa8	000010fc	streqd	r1, [r0], -ip
__ZN3dsp15LocationMessage13set_timestampEx:
0000afac	e92d4080	stmdb	sp!, {r7, lr}
0000afb0	e28d7000	add	r7, sp, #0	; 0x0
0000afb4	e24dd00c	sub	sp, sp, #12	; 0xc
0000afb8	e58d0008	str	r0, [sp, #8]
0000afbc	e88d0006	stmia	sp, {r1, r2}
0000afc0	e59d0008	ldr	r0, [sp, #8]
0000afc4	e3a01000	mov	r1, #0	; 0x0
0000afc8	e59f3020	ldr	r3, [pc, #32]	; 0xaff0
0000afcc	e08f3003	add	r3, pc, r3
0000afd0	e5933000	ldr	r3, [r3]
0000afd4	e12fff33	blx	r3
0000afd8	e59d1008	ldr	r1, [sp, #8]
0000afdc	e89d000c	ldmia	sp, {r2, r3}
0000afe0	e581200c	str	r2, [r1, #12]
0000afe4	e5813010	str	r3, [r1, #16]
0000afe8	e247d000	sub	sp, r7, #0	; 0x0
0000afec	e8bd8080	ldmia	sp!, {r7, pc}
0000aff0	00001098	muleq	r0, r8, r0
__ZN3dsp15LocationMessage12set_latitudeEd:
0000aff4	e92d4090	stmdb	sp!, {r4, r7, lr}
0000aff8	e28d7004	add	r7, sp, #4	; 0x4
0000affc	e24dd00c	sub	sp, sp, #12	; 0xc
0000b000	e58d0008	str	r0, [sp, #8]
0000b004	e88d0006	stmia	sp, {r1, r2}
0000b008	e59d0008	ldr	r0, [sp, #8]
0000b00c	e3a01001	mov	r1, #1	; 0x1
0000b010	e59f3020	ldr	r3, [pc, #32]	; 0xb038
0000b014	e08f3003	add	r3, pc, r3
0000b018	e5933000	ldr	r3, [r3]
0000b01c	e12fff33	blx	r3
0000b020	e59d2008	ldr	r2, [sp, #8]
0000b024	e89d0018	ldmia	sp, {r3, r4}
0000b028	e5823014	str	r3, [r2, #20]
0000b02c	e5824018	str	r4, [r2, #24]
0000b030	e247d004	sub	sp, r7, #4	; 0x4
0000b034	e8bd8090	ldmia	sp!, {r4, r7, pc}
0000b038	00001050	andeq	r1, r0, r0, asr r0
__ZN3dsp15LocationMessage13set_longitudeEd:
0000b03c	e92d4090	stmdb	sp!, {r4, r7, lr}
0000b040	e28d7004	add	r7, sp, #4	; 0x4
0000b044	e24dd00c	sub	sp, sp, #12	; 0xc
0000b048	e58d0008	str	r0, [sp, #8]
0000b04c	e88d0006	stmia	sp, {r1, r2}
0000b050	e59d0008	ldr	r0, [sp, #8]
0000b054	e3a01002	mov	r1, #2	; 0x2
0000b058	e59f3020	ldr	r3, [pc, #32]	; 0xb080
0000b05c	e08f3003	add	r3, pc, r3
0000b060	e5933000	ldr	r3, [r3]
0000b064	e12fff33	blx	r3
0000b068	e59d2008	ldr	r2, [sp, #8]
0000b06c	e89d0018	ldmia	sp, {r3, r4}
0000b070	e582301c	str	r3, [r2, #28]
0000b074	e5824020	str	r4, [r2, #32]
0000b078	e247d004	sub	sp, r7, #4	; 0x4
0000b07c	e8bd8090	ldmia	sp!, {r4, r7, pc}
0000b080	00001008	andeq	r1, r0, r8
__ZN3dsp15LocationMessage12set_altitudeEd:
0000b084	e92d4090	stmdb	sp!, {r4, r7, lr}
0000b088	e28d7004	add	r7, sp, #4	; 0x4
0000b08c	e24dd00c	sub	sp, sp, #12	; 0xc
0000b090	e58d0008	str	r0, [sp, #8]
0000b094	e88d0006	stmia	sp, {r1, r2}
0000b098	e59d0008	ldr	r0, [sp, #8]
0000b09c	e3a01003	mov	r1, #3	; 0x3
0000b0a0	e59f3020	ldr	r3, [pc, #32]	; 0xb0c8
0000b0a4	e08f3003	add	r3, pc, r3
0000b0a8	e5933000	ldr	r3, [r3]
0000b0ac	e12fff33	blx	r3
0000b0b0	e59d2008	ldr	r2, [sp, #8]
0000b0b4	e89d0018	ldmia	sp, {r3, r4}
0000b0b8	e5823024	str	r3, [r2, #36]
0000b0bc	e5824028	str	r4, [r2, #40]
0000b0c0	e247d004	sub	sp, r7, #4	; 0x4
0000b0c4	e8bd8090	ldmia	sp!, {r4, r7, pc}
0000b0c8	00000fc0	andeq	r0, r0, r0, asr #31
__ZN3dsp15LocationMessage22set_horizontalaccuracyEd:
0000b0cc	e92d4090	stmdb	sp!, {r4, r7, lr}
0000b0d0	e28d7004	add	r7, sp, #4	; 0x4
0000b0d4	e24dd00c	sub	sp, sp, #12	; 0xc
0000b0d8	e58d0008	str	r0, [sp, #8]
0000b0dc	e88d0006	stmia	sp, {r1, r2}
0000b0e0	e59d0008	ldr	r0, [sp, #8]
0000b0e4	e3a01004	mov	r1, #4	; 0x4
0000b0e8	e59f3020	ldr	r3, [pc, #32]	; 0xb110
0000b0ec	e08f3003	add	r3, pc, r3
0000b0f0	e5933000	ldr	r3, [r3]
0000b0f4	e12fff33	blx	r3
0000b0f8	e59d2008	ldr	r2, [sp, #8]
0000b0fc	e89d0018	ldmia	sp, {r3, r4}
0000b100	e582302c	str	r3, [r2, #44]
0000b104	e5824030	str	r4, [r2, #48]
0000b108	e247d004	sub	sp, r7, #4	; 0x4
0000b10c	e8bd8090	ldmia	sp!, {r4, r7, pc}
0000b110	00000f78	andeq	r0, r0, r8, ror pc
__ZN3dsp15LocationMessage20set_verticalaccuracyEd:
0000b114	e92d4090	stmdb	sp!, {r4, r7, lr}
0000b118	e28d7004	add	r7, sp, #4	; 0x4
0000b11c	e24dd00c	sub	sp, sp, #12	; 0xc
0000b120	e58d0008	str	r0, [sp, #8]
0000b124	e88d0006	stmia	sp, {r1, r2}
0000b128	e59d0008	ldr	r0, [sp, #8]
0000b12c	e3a01005	mov	r1, #5	; 0x5
0000b130	e59f3020	ldr	r3, [pc, #32]	; 0xb158
0000b134	e08f3003	add	r3, pc, r3
0000b138	e5933000	ldr	r3, [r3]
0000b13c	e12fff33	blx	r3
0000b140	e59d2008	ldr	r2, [sp, #8]
0000b144	e89d0018	ldmia	sp, {r3, r4}
0000b148	e5823034	str	r3, [r2, #52]
0000b14c	e5824038	str	r4, [r2, #56]
0000b150	e247d004	sub	sp, r7, #4	; 0x4
0000b154	e8bd8090	ldmia	sp!, {r4, r7, pc}
0000b158	00000f30	andeq	r0, r0, r0, lsr pc

;--------------------------------
;Assembly Program: Virtual Pet  |
;Target: Linux x86-64			|
;Assembler: NASM				|
;Developer: Skeome				|
; -------------------------------

section .data
	; --- SYSTEM CONSTANTS ---
	EXIT_SUCCESS equ 0
	EXIT_FAILURE equ 1
	STDIN equ 0
	STDOUT equ 1
	SYS_READ equ 0
	SYS_WRITE equ 1
	SYS_EXIT equ 60
	SYS_NANOSLEEP equ 35
    SYS_RT_SIGACTION equ 13     ; Syscall for setting up signal handlers

    ; --- SIGNAL HANDLING CONSTANTS ---
    SIGINT equ 2                ; Signal 2 = Interrupt (Ctrl+C)
    SA_RESTORER equ 0x04000000  ; Required flag for rt_sigaction
    
	; --- ANSI COLOR CODES (Demoscene Flair / Optimized Pixel Rendering) ---
	ANSI_RESET db 0x1B, "[0m"
	LEN_RESET equ $ - ANSI_RESET
	
    ; Foreground Status Colors
	ANSI_RED db 0x1B, "[31m"
	LEN_RED equ $ - ANSI_RED
	ANSI_GREEN db 0x1B, "[32m"
	LEN_GREEN equ $ - ANSI_GREEN
	ANSI_YELLOW db 0x1B, "[33m"
	LEN_YELLOW equ $ - ANSI_YELLOW
    
    ; Pixel Rendering Colors (Background Color for Pixel Effect)
    ; Use black background for "OFF" pixel
    ANSI_PIXEL_OFF db 0x1B, "[40m  " ; Black background + 2 spaces (the pixel block)
    LEN_PIXEL_OFF equ $ - ANSI_PIXEL_OFF
    
    ; Use white background for "ON" pixel (or any solid color for the pet)
    ANSI_PIXEL_ON db 0x1B, "[47m  "  ; White background + 2 spaces (the pixel block)
    LEN_PIXEL_ON equ $ - ANSI_PIXEL_ON
    
    ; Clear Screen and Move Cursor to Home
    ANSI_CLEAR_SCREEN db 0x1B, "[2J", 0x1B, "[H" ; \e[2J (Clear), \e[H (Home)
    LEN_CLEAR_SCREEN equ $ - ANSI_CLEAR_SCREEN

	; --- TEXT MESSAGES ---
	msg_title db '--- V-PET: Hatchling Console ---', 0xA, 0xA
	len_title equ $ - msg_title

    msg_status_hdr db 0xA, 0xA, "STATUS:", 0xA
    len_status_hdr equ $ - msg_status_hdr
    
    msg_health_label db "  HEALTH: "
    len_health_label equ $ - msg_health_label
    
    msg_hunger_label db "  HUNGER: "
    len_hunger_label equ $ - msg_hunger_label
    
    msg_age_label db "  AGE: "
    len_age_label equ $ - msg_age_label
    
    msg_strength_label db "  STRENGTH: "
    len_strength_label equ $ - msg_strength_label
    
    msg_menu db 0xA, "--- ACTIONS ---", 0xA
             db "1. Feed | 2. Train | 3. Quit", 0xA
             db "Choice: "
    len_menu equ $-msg_menu
    
    msg_action_feed db "ACTION: Delicious food consumed. +10 Health!", 0xA
    len_action_feed equ $-msg_action_feed
    
    msg_action_train db "ACTION: Training complete. Strength +5!", 0xA
    len_action_train equ $-msg_action_train
    
    msg_action_invalid db "ACTION: Invalid choice. Try again.", 0xA
    len_action_invalid equ $-msg_action_invalid

    msg_event_found db "EVENT: A wild event occurred!", 0xA
    len_event_found equ $ - msg_event_found
    
    msg_evolution db 0xA, 0x1B, "[36m--- EVOLVED! ---", 0x1B, "[0m", 0xA
    len_evolution equ $ - msg_evolution
    
    msg_error db 'ERROR: Program encountered a critical failure.', 0xA
	len_error equ $ - msg_error

	msg_newline db 0xA
	len_newline equ $ - msg_newline
    
    msg_death db 0xA, 0xA, 0x1B, "[31m--- DECEASED ---", 0x1B, "[0m", 0xA ; Red death message
    len_death equ $-msg_death

	; --- PET ASCII ART SPRITES (8x8 Hex Matrices for Optimization) ---
    ; Each sprite is 8 bytes, representing 8 rows of 8 bits (pixels).
    
    ; Stage 0: EGG 
    sprite_egg db 0x00, 0x3C, 0x7E, 0x7E, 0x7E, 0x3C, 0x00, 0x00
    len_sprite_egg equ 8 ; Length is 8 bytes
    
    ; Stage 1: BABY (Happy Face)
    sprite_baby db 0x00, 0x3C, 0x42, 0x99, 0xFF, 0x99, 0x42, 0x00
    len_sprite_baby equ 8
    
    ; Stage 2: CHILD (Training Stance / Taller)
    sprite_child db 0x7E, 0x3C, 0x18, 0x7E, 0x18, 0x18, 0x42, 0x00
    len_sprite_child equ 8

    ; Stage 3: ADULT (Placeholder for future definition)
    sprite_adult db 0xAA, 0x55, 0xAA, 0x55, 0xAA, 0x55, 0xAA, 0x55
    len_sprite_adult equ 8

	; --- TIME STRUCTURE (for nanosleep) ---
	; struct timespec { long tv_sec; long tv_nsec; };
	; We will sleep for 1 second per cycle.
	sleep_req_sec dq 1				; tv_sec = 1
	sleep_req_nsec dq 0      		; tv_nsec = 0
    
    ; --- LCG CONSTANTS (for PRNG) ---
    LCG_MULTIPLIER dq 6364136223846793005
    LCG_INCREMENT dq 1442695040888963407

    ; --- SIGACTION STRUCTURE ---
    ; struct sigaction { void (*sa_handler)(int); unsigned long sa_flags; ... }
    ; We only need the handler address and flags.
    sa_handler dq clean_exit        ; Function to call (clean_exit)
    sa_flags dq 0         ; <--- FIX: Reverted to 0 as per user's correct implementation
    sa_mask dq 0                    ; Signal mask (sa_mask[0] = 8 bytes)
    sa_restorer dq 0                ; sa_restorer (8 bytes)

section .bss
	; --- CORE PET STATE (32-bit DWORDS) ---
	pet_health resd 1   			; 0-100 (100 = full)
	pet_hunger resd 1   			; 0-100 (100 = starved)
	pet_age    resd 1   			; Game cycles elapsed
	pet_stage  resd 1   			; 0=Egg, 1=Baby, 2=Child, 3=Adult, etc.
    pet_strength resd 1             ; 0-100 (New stat for training/evolution)

	; --- RANDOM SEED (64-bit QWORD) ---
	prng_seed resq 1    			; Used for generating random events/battles.

	; --- I/O BUFFERS ---
	input_choice resb 4 			; Buffer for reading menu choice + newline
	num_buffer resb 10  			; Buffer for ASCII conversion of multi-digit numbers

	; --- TIME REMAINING (Required by nanosleep syscall) ---
	sleep_rem resb 16   			; Placeholder for remaining time structure

section .text
	global _start

; --- UTILITY ROUTINES -------------------------------------------------

; ----------------------------------------------------------------------
; ROUTINE: print_string
; Prints a string of a given length to stdout.
; Args:
;	rdi: Address of the string to print
;	rsi: Length of the string to print
; ----------------------------------------------------------------------
print_string:
	push rbp
	mov rbp, rsp
	push rdx                		; Save RDX (length) for syscall

	; rdi (address) and rsi (length) are set by caller
	mov rdx, rsi            		; Syscall arg 3: length
	mov rsi, rdi            		; Syscall arg 2: address
	mov rax, SYS_WRITE      		; Syscall 1 (write)
	mov rdi, STDOUT         		; Syscall arg 1: stdout
	syscall
    
	pop rdx
	mov rsp, rbp
	pop rbp
	ret

; ----------------------------------------------------------------------
; ROUTINE: print_num
; Converts a 32-bit integer (in EAX) to a string and prints it to stdout.
; Args:
;	eax: The number to print (32-bit)
;	Uses num_buffer (in .bss) to store the reversed digits.
; ----------------------------------------------------------------------
print_num:
	push rbp
	mov rbp, rsp
	push rbx                			; Save caller-saved registers
	push rcx
	push rdx
	push rsi

	mov rsi, num_buffer     			; Use buffer as scratchpad
	mov ecx, 0              			; Digit counter (length of the number string)
	mov ebx, 10             			; Divisor (10)
    
    ; Handle case where number is 0 (to avoid infinite loop if EAX=0)
    cmp eax, 0
    jne .divide_loop
    
    ; If EAX is 0, manually set first digit to '0'
    mov byte [rsi], '0'
    mov ecx, 1
    jmp .print_setup

.divide_loop:
	; Use 32-bit division: IDIV uses EDX:EAX as dividend.
	mov edx, 0              			; Clear EDX (high 32 bits of dividend)
	div ebx                 			; EAX = quotient, EDX = remainder (the digit)

	; Convert digit (EDX) to ASCII and store it in the buffer (reversed)
	add dl, '0'             			; Convert remainder in DL to ASCII
	mov [rsi+rcx], dl       			; Store ASCII digit
	inc ecx                 			; Increment digit counter
	cmp eax, 0              			; Check if quotient is zero
	jnz .divide_loop        			; Loop if quotient is not zero

; --- Print the digits from the buffer (which are currently reversed) ---
.print_setup:
	; RCX holds the count (length of number string)
	mov rbx, rcx            			; <--- FIX: Use RBX for loop counter
	mov rsi, num_buffer     			; RSI = starting address
	add rsi, rbx            			; Point RSI to the byte AFTER the last digit
	dec rsi                 			; Point RSI to the last digit (first to print)

.print_loop:
	; Print the character pointed to by RSI
	mov rax, SYS_WRITE
	mov rdi, STDOUT
	mov rdx, 1              			; Length is 1 byte
	syscall                 			; <--- This clobbers RCX, but not RBX

	dec rsi                 			; Move to the previous character
	dec rbx                 			; <--- FIX: Decrement RBX counter
	jnz .print_loop         			; <--- FIX: Jump based on RBX

	pop rsi                 			; Restore caller-saved registers
	pop rdx
	pop rcx
	pop rbx
	mov rsp, rbp
	pop rbp
	ret
; ----------------------------------------------------------------------

; --- DISPLAY ROUTINES -------------------------------------------------

; ----------------------------------------------------------------------
; ROUTINE: display_stat (Internal Helper)
; Prints a label, applies a color based on value, prints the number, and resets color.
; Args:
;   rdi: Label string address
;   rsi: Label string length
;   edx: 32-bit number value (e.g., pet_health)
; ----------------------------------------------------------------------
display_stat:
    push rbp
    mov rbp, rsp
    push rsi                ; Save label length
    push rdi                ; Save label address
    push rdx                ; Save number value (EDX)

    ; 1. Print Label
    mov rdx, rsi            ; RDX = length
    call print_string       ; Prints label (e.g., "  HEALTH: ")
    
    ; 2. Apply Color based on value (EDX) - Demoscene Visual Feedback
    pop rax                 ; Restore number value to EAX
    push rax                ; Save it again for print_num call
    
    cmp eax, 30
    jg .green_color         ; If value > 30, go to green check
    
    ; Value <= 30 (Critical/Low) -> RED
    mov rdi, ANSI_RED
    mov rsi, LEN_RED
    call print_string
    jmp .print_number
    
.green_color:
    cmp eax, 70
    jge .green_set          ; If value >= 70, go to green
    
    ; Value 31-69 (Warning/Normal) -> YELLOW
    mov rdi, ANSI_YELLOW
    mov rsi, LEN_YELLOW
    call print_string
    jmp .print_number
    
.green_set:
    ; Value 70-100 (Good/Full) -> GREEN
    mov rdi, ANSI_GREEN
    mov rsi, LEN_GREEN
    call print_string

.print_number:
    ; 3. Print Number
    pop rax                 ; Restore number to EAX
    call print_num
    
    ; 4. Reset Color
    mov rdi, ANSI_RESET
    mov rsi, LEN_RESET
    call print_string
    
    ; 5. Print Newline
    mov rdi, msg_newline
    mov rsi, len_newline
    call print_string
    
    pop rdi                 ; Restore original label address (from push rdi)
    pop rsi                 ; Restore original label length (from push rsi)
    mov rsp, rbp
    pop rbp
    ret
; ----------------------------------------------------------------------

; ----------------------------------------------------------------------
; ROUTINE: display_art
; Prints the 8x8 hexadecimal sprite corresponding to the current pet_stage.
; This routine iterates over the 8 bytes (rows) and 8 bits (pixels)
; and uses ANSI color blocks for optimized "pixel" rendering.
; ----------------------------------------------------------------------
display_art:
    push rbp
    mov rbp, rsp
    push rax                ; Holds current byte (row data)
    push rbx                ; Holds inner loop counter (8 pixels)
    push rcx                ; Holds outer loop counter (8 rows)
    push rdx
    push rsi                ; Holds sprite data pointer
    push rdi
    push r12                ; <--- Save R12 (callee-saved, safe for inner loop)
    
    ; 1. Determine which sprite pointer to use based on pet_stage
    mov eax, [pet_stage]    ; Load current stage (0, 1, 2, ...)
    mov rsi, sprite_egg     ; Default to Egg (Stage 0)
    
    cmp eax, 1
    je .set_baby
    
    cmp eax, 2
    je .set_child
    
    cmp eax, 3
    jge .set_adult          ; Use Adult for stage >= 3
    jmp .start_render       ; Use default Egg
    
.set_baby:
    mov rsi, sprite_baby
    jmp .start_render
    
.set_child:
    mov rsi, sprite_child
    jmp .start_render

.set_adult:
    mov rsi, sprite_adult
    
.start_render:
    mov rbx, 8              ; <--- FIX: Outer loop counter: 8 rows (RBX is callee-saved)
    
.row_loop:
    mov al, [rsi]           ; AL = current 8-bit row data
    mov r12, 8              ; <--- FIX: Inner loop counter: 8 pixels (R12 is callee-saved)
    
.pixel_loop:
    shl al, 1               ; Shift AL left by 1. MSB moves into the Carry Flag (CF)
    jnc .pixel_off          ; If CF=0 (No Carry), the pixel is OFF (Black)
    
    ; --- PIXEL ON (1) ---
    mov rdi, ANSI_PIXEL_ON
    mov rdx, LEN_PIXEL_ON
    call print_string       ; Syscall clobbers RCX, but not RBX or R12
    jmp .next_pixel

.pixel_off:
    ; --- PIXEL OFF (0) ---
    mov rdi, ANSI_PIXEL_OFF
    mov rdx, LEN_PIXEL_OFF
    call print_string       ; Syscall clobbers RCX, but not RBX or R12

.next_pixel:
    dec r12                 ; <--- FIX: Decrement inner counter
    jnz .pixel_loop         ; <--- FIX: Jump based on inner counter
    
    ; --- ROW END ---
    mov rdi, ANSI_RESET     ; Reset color/background after row
    mov rdx, LEN_RESET
    call print_string
    
    mov rdi, msg_newline    ; Print newline to start next row
    mov rdx, len_newline
    call print_string
    
    inc rsi                 ; Move sprite pointer to the next row (byte)
    dec rbx                 ; <--- FIX: Decrement outer counter
    jnz .row_loop           ; <--- FIX: Jump based on outer counter
    
    pop r12                 ; <--- Restore R12
    pop rdi
    pop rsi
    pop rdx
    pop rcx
    pop rbx
    pop rax
    mov rsp, rbp
    pop rbp
    ret
; ----------------------------------------------------------------------

; ----------------------------------------------------------------------
; ROUTINE: display_status
; Clears the screen and prints the pet's art and status stats.
; ----------------------------------------------------------------------
display_status:
    push rbp
    mov rbp, rsp
    push rax ; Save registers
    push rdx
    push rdi
    push rsi

    ; 1. Clear Screen (Demoscene Effect)
    mov rdi, ANSI_CLEAR_SCREEN
    mov rsi, LEN_CLEAR_SCREEN
    call print_string

    ; 2. Display Pet Art
    call display_art

    ; 3. Display Status Header
    mov rdi, msg_status_hdr
    mov rsi, len_status_hdr
    call print_string
    
    ; 4. Display Health (Pass Health value in EDX)
    mov rdi, msg_health_label
    mov rsi, len_health_label
    mov edx, [pet_health]
    call display_stat

    ; 5. Display Hunger (Pass Hunger value in EDX)
    mov rdi, msg_hunger_label
    mov rsi, len_hunger_label
    mov edx, [pet_hunger]
    call display_stat

    ; 6. Display Age (Pass Age value in EDX)
    mov rdi, msg_age_label
    mov rsi, len_age_label
    mov edx, [pet_age]
    call display_stat
    
    ; 7. Display Strength (New Stat)
    mov rdi, msg_strength_label
    mov rsi, len_strength_label
    mov edx, [pet_strength]
    call display_stat
    
    pop rsi ; Restore registers
    pop rdi
    pop rdx
    pop rax
    mov rsp, rbp
    pop rbp
    ret
; ----------------------------------------------------------------------

; --- ACTION ROUTINES --------------------------------------------------

; ----------------------------------------------------------------------
; ROUTINE: apply_feed
; Reduces pet_hunger by 20 and increases pet_health by 10. Clamps limits.
; ----------------------------------------------------------------------
apply_feed:
    push rbp
    mov rbp, rsp
    push rax
    push rsi
    
    ; Hunger: Hunger -= 20, clamp minimum to 0
    mov esi, [pet_hunger]
    sub esi, 20
    mov eax, 0
    cmp esi, eax            ; Check if Hunger < 0
    cmovl esi, eax          ; If less, set ESI = 0 (clamped)
    mov [pet_hunger], esi
    
    ; Health: Health += 10, clamp maximum to 100
    mov esi, [pet_health]
    add esi, 10
    mov eax, 100
    cmp esi, eax            ; Check if Health > 100
    cmovg esi, eax          ; If greater, set ESI = 100 (clamped)
    mov [pet_health], esi
    
    ; Print feedback message
    mov rdi, msg_action_feed
    mov rsi, len_action_feed
    call print_string
    
    pop rsi
    pop rax
    mov rsp, rbp
    pop rbp
    ret
; ----------------------------------------------------------------------

; ----------------------------------------------------------------------
; ROUTINE: apply_train
; Increases pet_strength by 5. Clamps maximum to 100.
; ----------------------------------------------------------------------
apply_train:
    push rbp
    mov rbp, rsp
    push rax
    push rsi
    
    ; Strength: Strength += 5, clamp maximum to 100
    mov esi, [pet_strength]
    add esi, 5
    mov eax, 100
    cmp esi, eax            ; Check if Strength > 100
    cmovg esi, eax          ; If greater, set ESI = 100 (clamped)
    mov [pet_strength], esi
    
    ; Print feedback message
    mov rdi, msg_action_train
    mov rsi, len_action_train
    call print_string
    
    pop rsi
    pop rax
    mov rsp, rbp
    pop rbp
    ret
; ----------------------------------------------------------------------

; ----------------------------------------------------------------------
; ROUTINE: handle_input
; Displays menu, reads choice, executes corresponding action, or exits.
; ----------------------------------------------------------------------
handle_input:
    push rbp
    mov rbp, rsp
    push rdi
    push rsi
    push rdx
    
    ; 1. Print Menu
    mov rdi, msg_menu
    mov rsi, len_menu
    call print_string
    
    ; 2. Read User Input (1 byte + newline)
    mov rax, SYS_READ       ; Syscall 0
    mov rdi, STDIN          ; stdin
    mov rsi, input_choice
    mov rdx, 4              ; Read up to 4 bytes (choice + newline + nulls)
    syscall
    
    ; 3. Process Choice - Use the first byte of input buffer
    movzx rax, byte [input_choice] ; Load choice into AL, zero-extend to RAX
    
    cmp al, '1'
    je .feed_choice
    
    cmp al, '2'
    je .train_choice
    
    cmp al, '3'
    je .quit_requested
    
    ; Invalid Choice Handler
    mov rdi, msg_action_invalid
    mov rsi, len_action_invalid
    call print_string
    
    ; Default: Continue game loop
    xor eax, eax            ; Return 0 (continue)
    jmp .return
    
.feed_choice:
    call apply_feed
    xor eax, eax            ; Return 0 (continue)
    jmp .return
    
.train_choice:
    call apply_train
    xor eax, eax            ; Return 0 (continue)
    jmp .return

.quit_requested:
    mov eax, 1              ; Return 1 (quit)
    
.return:
    pop rdx
    pop rsi
    pop rdi
    mov rsp, rbp
    pop rbp
    ret
; ----------------------------------------------------------------------

; --- GAME LOGIC ROUTINES ----------------------------------------------

; ----------------------------------------------------------------------
; ROUTINE: update_state
; Decreases health, increases hunger, increases age, etc.
; ----------------------------------------------------------------------
update_state:
    push rbp
    mov rbp, rsp
    push rax
    push rsi
    
    ; --- AGE INCREMENT ---
    inc dword [pet_age]
    
    ; --- HUNGER DECAY ---
    mov esi, [pet_hunger]
    add esi, 5
    
    ; Clamp max hunger to 100
    mov eax, 100
    cmp esi, eax
    cmovg esi, eax
    mov [pet_hunger], esi
    
    ; --- HEALTH DECAY (Penalty if Hunger > 80) ---
    mov esi, [pet_hunger]
    mov eax, [pet_health]
    cmp esi, 80             ; Check if hunger is high
    jle .skip_health_penalty
    
    ; Health -= 5
    sub eax, 5
    
.skip_health_penalty:
    ; Clamp min health to 0
    mov esi, 0
    cmp eax, esi
    cmovl eax, esi          ; If less than 0, set EAX=0
    mov [pet_health], eax
    
    pop rsi
    pop rax
    mov rsp, rbp
    pop rbp
    ret
; ----------------------------------------------------------------------

; ----------------------------------------------------------------------
; ROUTINE: generate_random_number
; Implements a 64-bit LCG, returning a 32-bit random number (0-99) in EAX.
; This is a fast, highly optimized PRNG using native CPU instructions.
; Args: None
; Returns: EAX = Random number (0-99)
; ----------------------------------------------------------------------
generate_random_number:
    push rbp
    mov rbp, rsp
    push rbx
    push rdx
    push rdi
    
    ; 1. Load Current Seed (64-bit)
    mov rax, [prng_seed]
    
    ; 2. Mix with current Time-Stamp Counter (RDTSC) for entropy
    rdtsc                   ; RDX:RAX = 64-bit TSC
    xor [prng_seed], rax    ; Mix low TSC bits into seed
    mov rax, [prng_seed]    ; Reload the (now mixed) seed into RAX
    
    ; 3. LCG Update: Seed = (Seed * Multiplier) + Increment
    ; RAX = Seed, Multiplier in memory (LCG_MULTIPLIER)
    mul qword [LCG_MULTIPLIER]  ; RDX:RAX = RAX * LCG_MULTIPLIER (64x64=128 bit result)
    add rax, [LCG_INCREMENT]    ; Add Increment to the lower 64 bits (RAX)
    mov [prng_seed], rax        ; Store the new 64-bit seed (RAX)
    
    ; 4. Constrain the result to 0-99 (for percentage chances)
    ; Use the high part of the multiplication (RDX) as the random number
    mov ebx, 100            ; Divisor = 100
    mov eax, edx            ; Move high 32 bits (EDX) into EAX
    xor edx, edx            ; Clear EDX for 32-bit division
    div ebx                 ; EAX = quotient, EDX = Remainder (0-99)
    
    mov eax, edx            ; EAX now holds the random number (0-99)
    
    pop rdi
    pop rdx
    pop rbx
    mov rsp, rbp
    pop rbp
    ret
; ----------------------------------------------------------------------


; ----------------------------------------------------------------------
; ROUTINE: check_random_event
; Uses the PRNG to determine if a random event or battle occurs.
; Currently set for a 5% chance per game cycle.
; ----------------------------------------------------------------------
check_random_event:
    push rbp
    mov rbp, rsp
    push rdi
    push rsi
    push rax
    
    call generate_random_number ; EAX = random number (0-99)
    
    cmp eax, 5                  ; Check if Random Number is <= 5 (5% chance)
    jg .no_event                ; If > 5, skip the event
    
    ; --- EVENT TRIGGERED ---
    mov rdi, msg_event_found
    mov rsi, len_event_found
    call print_string           ; Notify the user
    
.no_event:
    pop rax
    pop rsi
    pop rdi
    mov rsp, rbp
    pop rbp
    ret
; ----------------------------------------------------------------------

; ----------------------------------------------------------------------
; ROUTINE: check_evolution
; Checks current pet state against evolution thresholds and updates pet_stage.
; Evolution Rules:
; Stage 0 (Egg) -> Stage 1 (Baby): Age >= 5
; Stage 1 (Baby) -> Stage 2 (Child): Age >= 15 AND Strength >= 30
; Stage 2 (Child) -> Stage 3 (Adult): Age >= 30 AND Strength >= 70
; ----------------------------------------------------------------------
check_evolution:
    push rbp
    mov rbp, rsp
    push rax
    push rdx
    
    mov eax, [pet_stage]
    
    ; --- Check Evolution from Stage 2 (Child) to Stage 3 (Adult) ---
    cmp eax, 2
    jne .check_stage_1
    
    mov edx, [pet_age]      ; EDX = Age
    cmp edx, 30             ; Check Age >= 30
    jl .no_evolution        ; If Age < 30, no evolution
    
    mov edx, [pet_strength] ; EDX = Strength
    cmp edx, 70             ; Check Strength >= 70
    jl .no_evolution        ; If Strength < 70, no evolution
    
    ; Success: Evolve to Stage 3
    mov dword [pet_stage], 3
    jmp .evolved

.check_stage_1:
    ; --- Check Evolution from Stage 1 (Baby) to Stage 2 (Child) ---
    cmp eax, 1
    jne .check_stage_0
    
    mov edx, [pet_age]      ; EDX = Age
    cmp edx, 15             ; Check Age >= 15
    jl .no_evolution        ; If Age < 15, no evolution
    
    mov edx, [pet_strength] ; EDX = Strength
    cmp edx, 30             ; Check Strength >= 30
    jl .no_evolution        ; If Strength < 30, no evolution
    
    ; Success: Evolve to Stage 2
    mov dword [pet_stage], 2
    jmp .evolved
    
.check_stage_0:
    ; --- Check Evolution from Stage 0 (Egg) to Stage 1 (Baby) ---
    cmp eax, 0
    jne .no_evolution       ; Only Egg evolves here
    
    mov edx, [pet_age]      ; EDX = Age
    cmp edx, 5              ; Check Age >= 5
    jl .no_evolution        ; If Age < 5, no evolution
    
    ; Success: Evolve to Stage 1
    mov dword [pet_stage], 1
    
.evolved:
    ; Print Evolution Message
    mov rdi, msg_evolution
    mov rsi, len_evolution
    call print_string
    
.no_evolution:
    pop rdx
    pop rax
    mov rsp, rbp
    pop rbp
    ret
; ----------------------------------------------------------------------

; ----------------------------------------------------------------------
; ROUTINE: check_death
; Checks if health is zero, and if so, initiates game over.
; ----------------------------------------------------------------------
check_death:
    push rbp
    mov rbp, rsp
    push rax
    
    mov eax, [pet_health]
    cmp eax, 0
    jnz .alive                  ; If health > 0, continue
    
    ; --- PET IS DEAD ---
    mov rdi, msg_death          ; Print red "DECEASED" message
    mov rsi, len_death
    call print_string
    
    ; Wait for user to read message (1 second)
    mov rax, SYS_NANOSLEEP
    mov rdi, sleep_req_sec
    mov rsi, sleep_rem
    syscall
    
    jmp clean_exit              ; Game Over
    
.alive:
    pop rax
    mov rsp, rbp
    pop rbp
    ret
; ----------------------------------------------------------------------


; ----------------------------------------------------------------------
; ROUTINE: setup_sigint_handler
; Sets up a signal handler for SIGINT (Ctrl+C) to call clean_exit.
; Uses SYS_RT_SIGACTION (syscall 13)
; ----------------------------------------------------------------------
setup_sigint_handler:
    push rbp
    mov rbp, rsp
    push rax
    push rdi
    push rsi
    push rdx
    push r10
    push r8
    
    ; Syscall arguments:
    ; RAX = SYS_RT_SIGACTION (13)
    ; RDI = signal number (SIGINT = 2)
    ; RSI = new action (address of struct sa_handler)
    ; RDX = old action (NULL / 0)
    ; R10 = size of sigset_t (8 bytes for 64-bit)
    
    mov rax, SYS_RT_SIGACTION
    mov rdi, SIGINT             ; Signal number (2)
    mov rsi, sa_handler         ; Pointer to new sigaction struct (sa_handler, sa_flags, sa_mask...)
    mov rdx, 0                  ; No need to retrieve old action
    mov r10, 8                  ; Size of sigset_t (8 bytes)
    mov r8, 0                   ; Restore frame (unused)
    syscall                     ; Execute syscall
    
    pop r8
    pop r10
    pop rdx
    pop rsi
    pop rdi
    pop rax
    mov rsp, rbp
    pop rbp
    ret
; ----------------------------------------------------------------------


; ----------------------------------------------------------------------
; ROUTINE: clean_exit
; Exits the program with a success status (0).
; ----------------------------------------------------------------------
clean_exit:
	mov rax, SYS_EXIT		; Syscall 60 (exit)
	mov rdi, EXIT_SUCCESS	; Exit status 0
	syscall
; ----------------------------------------------------------------------

; ----------------------------------------------------------------------
; ROUTINE: error_exit
; Displays an error message and exits the program with a failure status (1).
; ----------------------------------------------------------------------
error_exit:
    ; Print Error Message
	mov rdi, msg_error
	mov rsi, len_error
	call print_string

    ; Exit with Error Code
	mov rax, SYS_EXIT		; Syscall 60 (exit)
	mov rdi, EXIT_FAILURE	; Exit status 1
	syscall
; ----------------------------------------------------------------------


_start:
	; 1. Initialize Pet State
	mov dword [pet_health], 100
	mov dword [pet_hunger], 50
	mov dword [pet_age], 0
    mov dword [pet_strength], 0 ; New stat initialized
	mov dword [pet_stage], 0    ; Start at Egg stage
    
    ; 2. Initialize PRNG Seed
    rdtsc                       ; RDX:RAX = 64-bit time-stamp counter
    mov [prng_seed], rax        ; Use the initial TSC as the very first seed
    
    ; 3. Setup Signal Handler for Ctrl+C
    call setup_sigint_handler
    
    ; 4. Print Welcome Message
	mov rdi, msg_title
	mov rsi, len_title
	call print_string
    
.game_loop_start:
	; A. DISPLAY: Call display_status
    call display_status
    
	; B. INPUT: Call handle_input (User action: Feed, Train, Clean, Quit)
    call handle_input
    cmp eax, 1              ; Did handle_input return 1 (Quit)?
    je clean_exit           ; If yes, exit gracefully
    
	; C. EVENT CHECK: Call check_random_event (Uses PRNG_SEED)
    call check_random_event
    
	; D. EVOLUTION CHECK: Call check_evolution (T3 implemented)
    call check_evolution
    
	; E. UPDATE: Call update_state (Decay logic, Age++)
    call update_state
    
	; F. CHECK: Call check_death (If Health <= 0 or Age > MAX_AGE)
    call check_death

	; G. PAUSE: Syscall 35 (nanosleep) for 1 second.
    mov rax, SYS_NANOSLEEP          	; Syscall 35
    mov rdi, sleep_req_sec          	; Request time structure (1 second)
    mov rsi, sleep_rem              	; Remaining time structure (placeholder)
    syscall
    
	jmp .game_loop_start
    
; ----------------------------------------------------------------------
; --- BRAINSTORMING AREA ---
;
; The architecture is now defined. We have:
; 1. Data/State (.bss)
; 2. Print utility (print_num, print_string)
; 3. Visuals (ANSI codes, optimized 8x8 Hex Sprites)
; 4. Core Game Loop logic (A-G steps implemented with calls)
; 5. Randomness (generate_random_number, check_random_event)
; 6. User Actions (handle_input, apply_feed, apply_train)
; 7. Robustness (setup_sigint_handler for Ctrl+C)
; 
; Next immediate tasks:
;
; **T3. Evolution Logic:** Implemented in check_evolution based on age and pet_strength.
; **T4. Death Conditions:** Implemented in check_death.
; **T5. Random Event Logic:** Enhance `check_random_event` to actually apply effects (e.g., small health loss or gain).
; **T6. Battle System:** Define an enemy sprite and battle logic based on pet_strength.
;
;----------------------------------------------------------------------
;
; For further development and brainstorming, here are some ideas and steps to consider:
; The first thing we need to do is make a window to display the program in.
; This will require using Linux syscalls to create a terminal window.
;
; next, we need to create a simple background for the pet to exist in.
; This can be done using ASCII art or simple characters to represent the environment.
; This can also be done using ANSI color codes to make it more visually appealing.
;
; We also need to create the pet itself.
; This can be done by making an 8x8 pixel grid and filling it with hex values
; that represent the pet's appearance.
; We can start with monochrome sprites and later expand to color sprites using ANSI color codes.
; We can also expand to using sprite sheets for different animations (idle, happy, sad, eating, etc.)
; but a static sprite is fine for now.
;
; We need to create a way to display the pet's stats. Possibly a simple text display at the bottom of the window.
; 
; We could also order our menus in an early RPG style with a pointer to select options.
; This will require creating a simple menu system that can be navigated using the keyboard.
; (Arrow keys or WASD, Enter or Space to select, Backspace to cancel)
;
; (Alternatively, we could stick to the original/classic vpet experience and assign 'A' 'B' or 'C' to keyboard buttons)
; (Another alternative is the latest SCSA - or Super Complete Select Animation - Digivice Color style, with up/down to select, 'A' to confirm, 'B' to cancel)
; Nevertheless, we need to create a way to handle user input.
;
;
; We need to create a way to update the pet's stats over time.
; Stats are updated whenever the pet trains or is fed.
; Hunger increases over time, but health decreases if too much damage is taken.
; Age increases over time, and the pet evolves at certain age milestones.
; We also need to create a way to check for random events.
; These events can be positive (finding money to buy special decorative items) or negative (getting sick, random enemy battle, etc).
; Random events can be triggered based on the PRNG seed we set at the start of the program.
; They can also be influenced by real-world events, such as internet signals or system time.
; We can use the RDTSC instruction to get a time-stamp counter value and use that as a seed for our PRNG.
;
;
; Additional brainstorming ideas:
;
; 11. **Dynamic Time-Based Events**:
;     - Use the system clock to trigger events at specific times (e.g., feeding reminders every 5 minutes).
;     - Implement a "day-night" cycle to influence pet behavior (e.g., sleep at night).
;
; 12. **Happiness Mechanic**:
;     - Introduce a happiness stat that changes based on user interactions.
;     - Positive actions (feeding, playing) increase happiness, while neglect decreases it.
;     - Low happiness could lead to penalties (e.g., reduced health regeneration).
;
; 13. **Animation System**:
;     - Create simple animations for the pet (e.g., blinking, moving).
;     - Use ANSI escape codes to update the terminal display dynamically.
;
; 14. **Inventory System**:
;     - Allow the user to collect and use items (e.g., food, medicine, toys).
;     - Display the inventory in a menu and handle item usage.
;
; 15. **Sound Effects**:
;     - Explore using the Linux `beep` command or other methods to add simple sound effects.
;     - Example: Play a sound when the pet evolves or during battles.
;
; 16. **Advanced Random Events**:
;     - Add rare events with unique outcomes (e.g., finding a rare item, encountering a special pet).
;     - Use weighted probabilities to make some events more likely than others.
;
; 17. **Stat Decay Balancing**:
;     - Fine-tune the rate at which stats like hunger and health change over time.
;     - Ensure the game remains challenging but not frustrating for the player.
;
; 18. **Easter Eggs**:
;     - Add hidden features or messages that players can discover (e.g., secret pet stages).
;     - Trigger easter eggs based on specific inputs or rare random events.
;
; 19. **Multiplayer Interaction**:
;     - Explore the possibility of connecting two instances of the program for pet battles or trading.
;     - Use sockets or shared files for communication between instances.
;
; 20. **Achievements and Progress Tracking**:
;     - Track player accomplishments (e.g., "Fed pet 100 times", "Survived 30 days").
;     - Display achievements in a dedicated menu or at the end of the game.
;
;

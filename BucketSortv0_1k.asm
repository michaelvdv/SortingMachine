;  Interrupt works                                                                                                                                                                       
;  test loop to load conveyor output and led2 works


; BBBBBBBBBBBBBBBBB                                         kkkkkkkk                                     tttt             SSSSSSSSSSSSSSS                                               tttt          
; B::::::::::::::::B                                        k::::::k                                  ttt:::t           SS:::::::::::::::S                                           ttt:::t          
; B::::::BBBBBB:::::B                                       k::::::k                                  t:::::t          S:::::SSSSSS::::::S                                           t:::::t          
; BB:::::B     B:::::B                                      k::::::k                                  t:::::t          S:::::S     SSSSSSS                                           t:::::t          
; B::::B     B:::::Buuuuuu    uuuuuu      cccccccccccccccc k:::::k    kkkkkkk eeeeeeeeeeee    ttttttt:::::ttttttt    S:::::S               ooooooooooo   rrrrr   rrrrrrrrr   ttttttt:::::ttttttt    
; B::::B     B:::::Bu::::u    u::::u    cc:::::::::::::::c k:::::k   k:::::kee::::::::::::ee  t:::::::::::::::::t    S:::::S             oo:::::::::::oo r::::rrr:::::::::r  t:::::::::::::::::t    
; B::::BBBBBB:::::B u::::u    u::::u   c:::::::::::::::::c k:::::k  k:::::ke::::::eeeee:::::eet:::::::::::::::::t     S::::SSSS         o:::::::::::::::or:::::::::::::::::r t:::::::::::::::::t    
; B:::::::::::::BB  u::::u    u::::u  c:::::::cccccc:::::c k:::::k k:::::ke::::::e     e:::::etttttt:::::::tttttt      SS::::::SSSSS    o:::::ooooo:::::orr::::::rrrrr::::::rtttttt:::::::tttttt    
; B::::BBBBBB:::::B u::::u    u::::u  c::::::c     ccccccc k::::::k:::::k e:::::::eeeee::::::e      t:::::t              SSS::::::::SS  o::::o     o::::o r:::::r     r:::::r      t:::::t          
; B::::B     B:::::Bu::::u    u::::u  c:::::c              k:::::::::::k  e:::::::::::::::::e       t:::::t                 SSSSSS::::S o::::o     o::::o r:::::r     rrrrrrr      t:::::t          
; B::::B     B:::::Bu::::u    u::::u  c:::::c              k:::::::::::k  e::::::eeeeeeeeeee        t:::::t                      S:::::So::::o     o::::o r:::::r                  t:::::t          
; B::::B     B:::::Bu:::::uuuu:::::u  c::::::c     ccccccc k::::::k:::::k e:::::::e                 t:::::t    tttttt            S:::::So::::o     o::::o r:::::r                  t:::::t    tttttt
; BB:::::BBBBBB::::::Bu:::::::::::::::uuc:::::::cccccc:::::ck::::::k k:::::ke::::::::e                t::::::tttt:::::tSSSSSSS     S:::::So:::::ooooo:::::o r:::::r                  t::::::tttt:::::t
; B:::::::::::::::::B  u:::::::::::::::u c:::::::::::::::::ck::::::k  k:::::ke::::::::eeeeeeee        tt::::::::::::::tS::::::SSSSSS:::::So:::::::::::::::o r:::::r                  tt::::::::::::::t
; B::::::::::::::::B    uu::::::::uu:::u  cc:::::::::::::::ck::::::k   k:::::kee:::::::::::::e          tt:::::::::::ttS:::::::::::::::SS  oo:::::::::::oo  r:::::r                    tt:::::::::::tt
; BBBBBBBBBBBBBBBBB       uuuuuuuu  uuuu    cccccccccccccccckkkkkkkk    kkkkkkk eeeeeeeeeeeeee            ttttttttttt   SSSSSSSSSSSSSSS      ooooooooooo    rrrrrrr                      ttttttttttt  
; 
; Group 7
; 2IO70 - DBL Embedded Systems - TU/e
; Q4 2016
; BucketSort

@DATA
			; 	INPUTS
      pushBtn					DW			0				;
      lightTrap1			DW			0				;
      colourDetect1		DW			0				;
      lightTrap2			DW			0				;
      colourDetect2		DW			0				;
      buckBtn					DW			0				;
      startPause			DW			0				;
      
      ; 	OUTPUTS
      conveyor				DW			0				; OUT2
      pusher					DW			0				; OUT3
      led2						DW			0				; OUT4
      led1						DW			0				; OUT5
      bucketR					DW			0				; OUT6   \ H-Bridge
      bucketL					DW			0				; OUT7   / 
     
     
      
      ;	 TIMER ENABLE VARIABLES
      timer_enable		DW			0				;
      timer2_enable		DW			0				;
      timerP_enable		DW			0				;
      
      ;	 TIMEOUT FLAGS
      timeout					DW			0				;
      timeout2				DW			0				;
      timeoutP				DW			0				;
      
      ;	 TIMER COUNTERS
      timer_counter		DW			0				;
      timer2_counter	DW			0				;
      timerP_counter	DW			0				;
      
      ; DELAY VARIABLES
      delay						DW			0				;
      delay2					DW			0				;
      
      ;		STATE VARIABLES
      currentState		DW			0				;
      prevState				DW			0				;
      
      ; 	BOOKKEEPING VARIABLES
      pressed					DW			0				;
      button_hit			DW			0				;
      whiteDisc				DW			0				;
      outputBits			DW			0				;
      pwmBits					DW			0				;
      
      ;		COUNTERS
      countBlack			DW			0				;
      countWhite			DW			0				;
      
      ;		PWM DATA
      pwm_counter			DW			0				;

			testCount				DW			0				;

			;	Display buffer
      ; Display					DS		NDIGIT    ; display state

@CODE
      IOAREA		  		EQU		 -16			;	 IOAREA
			INPUT		    		EQU			7			  ;	 INPUT BUTTONS
			OUTPUT		  		EQU			11			;	 OUTPUT PINS
			TIMER		    		EQU			13			;	 TIMER REGISTER
			ADCONVS		  		EQU 		6       ;  A/D converters
      DIGIT	      		EQU    -7       ;  relative position of the 7-segment display's digit selector
      SEGMENT      		EQU    -8       ;  relative position of the 7-segment display's segments
      NDIGIT					EQU			6				;	 number of digits      
      ; definition booleans
      TRUE						EQU			1				; defines boolean values 'TRUE' and 'FALSE'
      FALSE						EQU			0				;
      
      ; statespace
      INITIAL					EQU			0				;
      START						EQU			1				;
      PUSHER					EQU			2				;
      DETECT					EQU			3				;
      SORT						EQU			4				;
      RESET						EQU			5				;
      PAUSE						EQU			6				;
      
      ; 	DELAY CONSTANTS 
      TIMEUNIT				EQU			10			; our interrupt is triggered once every ms
      INIT_DELAY			EQU			2				;
      ABORT_DELAY			EQU			2000		;
      SYSCHECK_DELAY	EQU			2				;
      PUSH_DELAY			EQU			900			;
      DETECT1_DELAY		EQU			1000		;
      DETECT2_DELAY		EQU			500			;
      BUCKET_DELAY		EQU			1000		;
      BUCKET2_DELAY		EQU			1000		;
      
      ;		PWM CONSTANTS
      pwmMotor1				EQU			10	     ;   
      pwmMotor2				EQU			5				 ;
      pwmMotor3				EQU			10			 ;
      pwmLeds					EQU			20			 ;
      

begin:LOAD 		    R0			timerintr	;		Get address of interrupt handler
			ADD 		    R0			R5				;		Add code base
			LOAD 		    R1			16				;		Attach timer interrupt
			STOR 		    R0			[R1]			;		Install timer isr
      
      ; set registers 
      LOAD				R0			0					;   R0 := null pointer
      LOAD				R1			0					;   R1 := workspace
      LOAD				R2			0					;   R2 := workspace
      LOAD				R3			0					;   R3 := workspace
      LOAD				R4			0					;   R4 := workspace
      LOAD				R5			IOAREA		;   R5 := IOAREA pointer
                                    ;   R6 := Global base pointer
                                    ;   R7 := Stack pointer
      ; reset timer to 0
			SUB			    R1			[R5 + TIMER]	;		get negative timer value
			ADD 		    R1			1							;		add 1 to timer value
			STOR		    R1			[R5 + TIMER]	;		set timer value
			SETI		    8											;   activate interrupt
      
			BRA			    test							;		go to main loop

;*************TESTS****************
test: ;LOAD				R4			[GB + testCount]	;amount of time the interrupt has been called
			LOAD				R4			[GB + currentState]	; load current state in arbitrary register for testing
			
      LOAD				R1			TRUE			;
      ;STOR				R1			[GB + led1];
      STOR				R1			[GB + led2];
      ;STOR				R1			[GB + pusher];
      STOR				R1			[GB + conveyor];
      ;STOR				R1			[GB + bucketR];
      
      BRS					motor1						;
      
      BRA					test							
;******* END OF TEST LOOP *************

;**********BEGIN OF MAIN***********
main: 
      LOAD				R4			[GB + currentState]	; load current state in arbitrary register for testing
      ;LOAD				R4			[GB + testCount]	  ; amount of time the interrupt has been called
      
			;LOAD				R1			TRUE			;
      ;STOR				R1			[GB + led1];
      ;STOR				R1			[GB + led2];

			BRS					pauseCheck				;
			BRS					motor1						;
      
switch: ; check the currentState, and branch accordingly
			LOAD				R1			[GB + currentState]	;
      CMP					R1			INITIAL		;
      BEQ					initial						;
      
      ;CMP					R1			START			;
      ;BEQ					start							;

      CMP					R1			PUSHER		;
      BEQ					push							;
      
      CMP					R1			DETECT		;
      BEQ					detect						;
      
      CMP					R1			SORT			;
      BEQ					sort							;
      
      CMP					R1			RESET			;
      BEQ					reset							;
      
      CMP					R1			PAUSE			;
      BEQ					pause							;

			BRA					main							; loop back

;*****************END OF MAIN****************
      
timerintr:
      ; pwm handling:
      ; update pwm counter
      LOAD				R1			[GB + testCount]
      ADD					R1			1
      STOR				R1			[GB + testCount]
      BRS					pwm_func 										;TEST
      BRA					in7								;

pwm_func:
      LOAD				R4			[GB + pwm_counter]	;
      ADD					R4			1					;
      MOD					R4			25				;
      STOR				R4			[GB + pwm_counter]	;
      
      RTS
      
      ; scan input values
in7:	LOAD				R1			[R5 + INPUT]	; retrieve input values
      LOAD				R2			R1				;
      AND					R2			%010000000;
      BNE					stor_in7
      LOAD				R2			TRUE			;
stor_in7:
			STOR				R2			[GB + startPause]	; 
      
in5:	LOAD				R2			R1				;
      AND					R2			%0100000;
      BNE					stor_in5
      LOAD				R2			TRUE			;
stor_in5:
			STOR				R2			[GB + buckBtn]	;

in4:	LOAD				R2			R1				;
      AND					R2			%010000;
      BNE					stor_in4
      LOAD				R2			TRUE			;
stor_in4:
			STOR				R2			[GB + colourDetect2]	;
      

in3:	LOAD				R2			R1				;
      AND					R2			%01000;
      BNE					stor_in3
      LOAD				R2			TRUE			;
stor_in3:
			STOR				R2			[GB + lightTrap2]	;


in2:	LOAD				R2			R1				;
      AND					R2			%0100;
      BNE					stor_in2
      LOAD				R2			TRUE			;
stor_in2:
			STOR				R2			[GB + colourDetect1]	;
      
      
in1:	LOAD				R2			R1				;
      AND					R2			%010;
      BNE					stor_in1
      LOAD				R2			TRUE			;
stor_in1:
			STOR				R2			[GB + lightTrap1]	;
      

in0:	LOAD				R2			R1				;
      AND					R2			%01;
      BNE					stor_in0
      LOAD				R2			TRUE			;
stor_in0:
			STOR				R2			[GB + pushBtn]	;
      
      ; update timers
timer:
			; check for PAUSE state
      LOAD				R1			[GB + currentState]	;
      CMP					R1			PAUSE			;
      BNE					timerP
      
			; check for timer_enable
			LOAD				R1			[GB + timer_enable]	;
			CMP					R1			TRUE			;
      BNE					timer_off
      
      ; increment timer counter
      LOAD				R1			[GB + timer_counter]	;
      ADD					R1			1					;
      STOR				R1			[GB + timer_counter]	;
      
      ; check if the delay has been reached
      LOAD				R2			[GB + delay]	;
      CMP					R1			R2				;
      BNE					timer2
       
      ; timeout execution
      LOAD				R1			TRUE			;
      STOR				R1			[GB + timeout]	;
      STOR				R0			[GB + timer_counter]	;
      LOAD				R1			FALSE			;
      STOR				R1			[GB + timer_enable]	;
      
timer_off:
			STOR				R0			[GB + timer_counter]	;
      
      
timer2:
						; check for timer2_enable
			LOAD				R1			[GB + timer2_enable]	;
			CMP					R1			TRUE			;
      BNE					timer2_off
      
      ; increment timer2 counter
      LOAD				R1			[GB + timer2_counter]	;
      ADD					R1			1					;
      STOR				R1			[GB + timer2_counter]	;
      
      ; check if the delay2 has been reached
      
      LOAD				R2			[GB + delay2]	;
      CMP					R1			R2				;
      BNE					timerP						;
       
      ; timeout2 execution
      LOAD				R1			TRUE			;
      STOR				R1			[GB + timeout2]	;
      STOR				R0			[GB + timer2_counter]	;
      LOAD				R1			FALSE			;
      STOR				R1			[GB + timer2_enable]	;
      
timer2_off:
			STOR				R0			[GB + timer2_counter]	;
      
timerP:
						; check for timerP_enable
			LOAD				R1			[GB + timerP_enable]	;
			CMP					R1			TRUE			;
      BNE					timerP_off
      
      ; increment timerP counter
      LOAD				R1			[GB + timerP_counter]	;
      ADD					R1			1					;
      STOR				R1			[GB + timerP_counter]	;
      
      ; check if the abort_delay has been reached
      LOAD				R2			ABORT_DELAY	;
      CMP					R1			R2				;
      BNE					display
       
      ; timeoutP execution
      LOAD				R1			TRUE			;
      STOR				R1			[GB + timeoutP]	;
      STOR				R0			[GB + timerP_counter]	;
      LOAD				R1			FALSE			;
      STOR				R1			[GB + timerP_enable]	;
      
timerP_off:
			STOR				R0			[GB + timerP_counter]	;
      
display:
			LOAD				R1			TIMEUNIT		;
      STOR				R1			[R5 + TIMER]	;
      
      ;end interrupt
      SETI				8
      RTE
      
;***************UPDATE OUTPUTS**************
; calculate results per output
motor1: ; enables pusher
			LOAD				R2			[GB + pwm_counter]
			LOAD				R3			[GB + pusher]	;
			CMP					R3			TRUE			;
      BNE					motor2
      LOAD				R1			%000000000;
			LOAD				R3			pwmMotor1	;
      CMP					R2			R3				;
      BPL 				motor2
      OR					R1			%000100000;
      
      
motor2:	; conveyor belt
			LOAD				R3			[GB + conveyor]	;
      CMP					R3			TRUE			;
      BNE					motor3R						;
			LOAD				R3			pwmMotor2	;
      CMP					R2			R3				;
      BPL					motor3R
      OR					R1			%000000100;
      
motor3R:
      LOAD				R3			[GB + bucketR]	;
      CMP					R3			TRUE			;
      BNE					motor3L
			LOAD				R3			pwmMotor3	;
      CMP					R2			R3				;
      BPL					motor3L
      OR					R1			%001000000;
      
motor3L:
      LOAD				R3			[GB + bucketL]	;
      CMP					R3			TRUE			;
      BNE					led1							;
			LOAD				R3			pwmMotor3	;
      CMP					R2			R3				;
      BNE					led1							;
      OR					R1			%010000000;
      
led1:	LOAD				R3			[GB + led1]
			CMP					R3			TRUE			;
      BNE					led2
      ;LOAD				R3			pwmLeds		;
      ;CMP					R2			R3				;
      ;BNE					led2							;
      OR					R1			%000000010;
      
led2:	LOAD				R3			[GB + led2]	;
      CMP					R3			TRUE			;
      BNE					stor_out						;
			LOAD				R3			pwmLeds	;
      CMP					R2			R3				;
      BPL					stor_out
      OR					R1			%000100000;
      
      ; store results in RAM
stor_out:
			AND					R1			%111111110
			STOR				R1			[R5 + OUTPUT]	;
      RTS
      
      
;***************DISABLE OUTPUTS*************
disableOutputs:
			LOAD				R1			%000000000;
      STOR				R1			[R5	+ OUTPUT]	;
      RTS
      
;***************PAUSE CHECK  ***************
pauseCheck:
			; check startPause
			LOAD				R1			[GB + startPause]	;
      CMP					R1			TRUE			;
      BNE					releaseCheck			;
      
      ; enable pause timer if pressed
      LOAD				R1			TRUE			;
      STOR				R1			[GB + timerP_enable]	;
      
releaseCheck:
			; check if pause timer is enabled
      LOAD				R1			[GB + timerP_enable]	;
      CMP					R1			TRUE			;
      BNE					abortCheck				;
      
      ; check if startPause has been released
      LOAD				R1			[GB + startPause]	;
      CMP					R1			FALSE			;
      BNE					abortCheck				;
      
      ; if so, disable the pause timer
      STOR				R0			[GB + timerP_enable]	;
      
      ; check for INITIAL state
      LOAD				R1			[GB + currentState]	;
      CMP					R1			INITIAL		;
      BNE					stateCheck				;
      
      ; if so, transition to PUSHER state
      LOAD				R1			PUSHER		;
      STOR				R1			[GB + currentState]	;
      BRA					pauseCheck_end		;
      
stateCheck:
			; check for PAUSE state				;
			CMP					R1			PAUSE			;
      BNE					pauseTrans				;
      
      ; if so, restore prevState
      LOAD				R1			[GB + prevState]	;
      STOR				R1			[GB + currentState]	;
      BRA					pauseCheck_end		;
      
pauseTrans:
			; transition to the PAUSE state, storing the prevState for later
			LOAD				R1			[GB + currentState]	;
      STOR				R1			[GB + prevState]	;
      LOAD				R1			PAUSE			;
      STOR				R1			[GB + currentState]	;
      BRA					pauseCheck_end		;
      
abortCheck:
			; reset timeoutP
      STOR				R0			[GB + timeoutP]	;
      
      ; abort
      ; display = "Abort";
      STOR				R0			[GB + currentState]	;
      
pauseCheck_end:
			RTS

;***************STATE INIITAL***************
initial:        BRS			disableOutputs					; disableOutputs()
                BRA			main										; 
               
    ;            LOAD		R1			INIT_DELAY			; set delay before displaying
   ;             STOR		R1			[GB + delay]		;

  ;              LOAD		R1			TRUE						; wait before displaying
 ;               STOR		R1			[GB + timer_enable] ;
            
;initialize_input :
                ; while (!timeout){}
                ;LOAD		R1			[GB + timeout]	
                ;CMP			R1			TRUE						; wait for time out
                ;BEQ			initialize_end					; if true, end loop

                ; loop
                ;BRA			initialize_input				;
            
;initialize_end :
            
                ;LOAD		R1			FALSE						; timeout = false
                ;STOR 		R1			[GB+timeout]		;

                ;if (startPause) 
                ;LOAD		R1			[GB+startPause]		
                ;CMP			R1			TRUE							
                ;BNE			check_sp_return						;	if false, skip code beneath and go back to main

                ;update state
                ;LOAD		R1			START
                ;STOR		R1			[GB + currentState]	;
                
                ;LOAD		R1			PUSHER
                ;STOR		R1			[GB + currentState] ;
                        
;check_sp_return :
                ; return
 ;               BRA     main
                
;***************STATE START***************
;start:					;set led1 and 2 to true
;                LOAD		R1			TRUE		
;                STOR		R1			[GB+led1]				
;                STOR		R1			[GB+led2]				
;
;                ; update delay
;                LOAD		R1			INIT_DELAY			; set delaytime for init
;                STOR		R1			[GB+delay]			;
;
;STloop:			; while (!timeout){}
;                LOAD		R1			[GB + timeout]	
;                CMP			R1			TRUE						; wait for time out
;                BEQ			nextST				;
;              	;finish loop
;                BRA			STloop			;
;
;nextST:
;
;								;set timeout to false
;                LOAD		R1			FALSE						; 
;                STOR		R1			[GB+timeout] ;
;
;                ; if (!lightTrap1)
;                LOAD		R1			[GB+lightTrap1]	; check lighttrap 1
;                CMP			R1			FALSE					;
;                BNE			lighttrap1_check_end		; if not false, skip if statement
;
;                ;update state
;                LOAD		R1			INITIAL
;                STOR		R1			[GB+currentState] ; if lighttrap 1 fails, go back to INITIAL state;
;
;                ; display = "Err03";   STILL NEEDS IMPLEMENTATION
;
;                ;set timer_enable to false
;                LOAD		R1			FALSE									
;                STOR		R1			[GB+timer_enable]			
;
;                BRA			lighttrap_timeout_check_end ; if above code is executed, skip the other 2 checks beneath
;
;  lighttrap1_check_end :
;
;                ;if (!lightTrap2)
;                LOAD		R1			[GB+lightTrap2]	; check lighttrap 2
;                CMP			R1			FALSE						;
;                BNE			lighttrap2_check_end		; if not false, skip if statement and go to else
;
;								;update state
;                LOAD		R1			INITIAL
;                STOR		R1 		  [GB+currentState] ; if lighttrap 2 fails, go back to INITIAL state
;
;                ; display = "Err04";   STILL NEEDS IMPLEMENTATION
;
;                ;set timer_enable to false
;                LOAD		R1			FALSE						
;                STOR		R1			[GB+timer_enable]	
;            
;  lighttrap2_check_end :
;
;                ;update state
;                LOAD		R1			PUSHER									;
;                STOR		R1			[GB+currentState] 			; if timeout is reached, go to PUSH state
;
;                ;disable leds and timeout
;                LOAD		R1			FALSE						
;                STOR		R1			[GB+led1]			
;                STOR		R1			[GB+led2]				
            
;	lighttrap_timeout_check_end :
      
;      					BRA 		main

;***************STATE PUSH***************

push:
							;display update

              ;set pusher to true
              LOAD  R1  TRUE			
              STOR  R1  [GB + pusher]		

              ;update delay
              LOAD  R1  [GB + PUSH_DELAY]	
              STOR  R1  [GB + delay]		

              ;set timer-enable = true 
              LOAD  R1	TRUE						
              STOR  R1	[GB + timer_enable] 

              ; if (pushBtn) 
              LOAD  R1  [GB + pushBtn]
              CMP   R1  TRUE
              BNE   nextPU1

              ;set button_hit to true 
              LOAD  R1  TRUE
              STOR  R1  [GB + button_hit]     
	      
              ; if (button_hit && !pushBtn)
nextPU1:      LOAD  R1  [GB + button_hit]
              LOAD  R2  [GB + pushBtn]
              CMP   R1  TRUE			
              BNE   nextPU2										;If not true, ignore if statement
              CMP   R2  FALSE
              BNE   nextPU2										;If not false, ignore if statement
              
              ;set pusher, button_hit, timer_enable to false
              LOAD  R1  FALSE		
              STOR  R1  [GB + pusher]	
              STOR  R1  [GB + button_hit]
              STOR  R1  [GB + timer_enable]

              ;update delay
              LOAD  R1  [GB + DETECT1_DELAY]	
              STOR  R1  [GB + delay]

              ;set timer_enable to true
              LOAD  R1  TRUE
              STOR  R1  [GB + timer_enable]

              ;update state
              ;LOAD  R1  DETECT
              LOAD	R1	INITIAL
              STOR  R1  [GB + currentState]
              
              ;skip else if if statement has been run 
              BRA   nextPU3

nextPU2:      ;else if (timeout)
    				  LOAD  R1  [GB + timeout]
              CMP   R1  TRUE
              BNE   nextPU3										;If not true, ignore if statement

              ;set timeout to false
              LOAD  R1  FALSE
              STOR  R1  [GB + timeout]

              ;update state
              LOAD  R1  INITIAL
              STOR  R1  [GB + currentState]

              ;display update?
              
nextPU3:      BRA   main

;***************STATE DETECT***************
detect:

              ;display update?
              ;***FILL***

              ;set leds and conveyor to true
              LOAD  R1  TRUE
              STOR  R1  [GB + led1]
              STOR  R1  [GB + led2]
              STOR  R1  [GB + conveyor]

              ; if (!lightTrap1) 
              LOAD  R1  [GB + lightTrap1]
              CMP   R1  FALSE									
              BNE   nextDE1										;If not false, ignore if statement

              ;set timer_counter to 0
              LOAD  R1  0
              STOR  R1  [GB + timer_counter]

              ;update delay
              LOAD  R1  DETECT2_DELAY	
              STOR  R1  [GB + delay]

              ; if (colourDetect1)
              LOAD  R1  [GB + colourDetect1]	;
              CMP   R1  TRUE									
              BNE   nextDEIf1									;If not true, ignore if statement inside if statement

              ;set WhiteDisc to true
              LOAD  R1  TRUE
              STOR  R1  [GB + whiteDisc]  

              ;update state
              LOAD  R1  SORT
              STOR  R1  [GB + currentState]

nextDEIf1:    
              ;update delay	  
              LOAD  R1  BUCKET_DELAY
              STOR  R1  [GB + delay]

              ;set timer_enable to true
              LOAD  R1  TRUE
              STOR  R1  [GB + timer_enable] 

              ;update state
              LOAD  R1  SORT
              STOR  R1  [GB + currentState]
              
              ;skip else if if statement has been run 
	            BRA   nextDE2
              
nextDE1:
              ;else if (timeout) 
              LOAD  R1  [GB + timeout]
              CMP   R1  TRUE
              BNE   nextDE2													;If not true, ignore if statement

              ;set timeout to false
              LOAD  R1  FALSE
              STOR  R1  [GB + timeout]

              ;update state	      
              LOAD  R1  INITIAL
              STOR  R1  [GB + currentState]

              ;display update?
              ;***FILL***
	     
nextDE2:			BRA   main

;***************STATE SORT***************     
 sort:     	
              	;BRS 		display										
                ;**FILL**
            
                ;set leds and conveyor to true
                LOAD		R1		TRUE								
                STOR		R1		[GB + led1]			
                STOR		R1		[GB + led2]			
                STOR		R1		[GB + conveyor]	
                
                ;if ((!bucketR) && (!bucketL))
              	LOAD		R1		[GB + bucketR]	;
                LOAD		R2		[GB + bucketL]	;
                OR			R1		R2									; if both are 0, then go into if, if not, go to next if
                CMP			R1		0										;
              	BNE			detect1										; if condition is not true, then go to detect statement
                
                ;update delay
                LOAD		R2		BUCKET_DELAY 
                STOR		R2		[GB + delay2]	
                
                ;set timer2_enable to true
                LOAD		R1		TRUE
                STOR		R1		[GB + timer2_enable]
                
                ;if (whiteDisc)
                LOAD		R1		[GB + whiteDisc]		;
                CMP			R1		TRUE								; check if whiteDisc
                BNE			black											; go to else
                
                ;set bucketR to true
                LOAD		R1	  TRUE								;
                STOR		R1		[GB + bucketR]			; activate bucketR
                BRA			detect1										; go to detect1 to skip else statement
                
     black:			;else
                LOAD		R1		TRUE								;
                STOR		R1		[GB + bucketL]			; activate bucketL, so move bucket to left.
              
		 detect1:		;if(!lighttrap2)
                LOAD		R1		[GB + lightTrap2]; 
                CMP			R1		FALSE;
                BNE			elseTimeout										; go to next if statement if condition is not sufficed.
               
                ;set timer_enable to false
                LOAD		R1		FALSE										
                STOR		R1		[GB	+ timer_enable]	
                
                ;if(!whiteDisc && colourDetect2)
                LOAD		R1		[GB + whiteDisc]			;
                LOAD		R2		[GB + colourDetect2]	;
                CMP			R1		FALSE										;	check if !whiteDisc
                BNE			elseIf2												; if this is not sufficed go out of statement
                CMP			R2		TRUE										; check if colourDetect2
                BNE			elseIf2										;
                
              	;update state	      
             	  LOAD  	R1 		INITIAL
             	  STOR  	R1  	[GB + currentState]
                
                ;display
                ;BRS 		display
                ;**FILL**
                
   							; else if (whiteDisc && !colourDetect)
   elseIf2 : 		LOAD		R1		[GB + whiteDisc]				;
              	LOAD		R2		[GB + colourDetect2]		;
                CMP			R1		TRUE										;
                BNE			elseTimeout										;if not true, skip statement
                CMP			R2		FALSE										;
                BNE			elseTimeout										;if not false, skip statement
                
                ;update state
                LOAD  	R1 		INITIAL
             	  STOR  	R1  	[GB + currentState]              
                
                ;display
                ;BRS 		display
                ;**FILL**
            
 								;else if (timeout)
 elseTimeout :  LOAD		R1		[GB + timeout]			;
                CMP			R1		TRUE								;
                BNE			ifBuck										; go out of else if(timeout), if condition is not sufficed
                
                ;set timeout to false
              	LOAD  	R1 		FALSE
                STOR 		R1  	[GB + timeout]

 								;update state
                LOAD  	R1 		INITIAL
             	  STOR  	R1  	[GB + currentState]
                
                ;display
                ;BRS 		display
                ;**FILL**
              
    						; if (buckBtn)
      ifBuck :  LOAD		R1		[GB + buckBtn]		  ; load buckBtn
                CMP			R1		TRUE								; if(buckBtn)
                BNE			elseTimeout2							; go to else if(timeout2)
                
                ;set timer2_enable to false
                LOAD		R1		FALSE								;
                STOR		R1		[GB + timer2_enable]; timer2_enable -> false
                
                ;if (bucketR)
                LOAD		R1		[GB + bucketR]	;	check bucketR
                CMP			R1		TRUE								;
                BNE			elseBucket								; if condition does not hold, go to else
                
                ;set bucketR to false and skip next else statement(s)
                LOAD    R1    FALSE
                STOR		R1		[GB + bucketR]	
                BRA 		contBuck
    
  elseBucket :  ;else if (bucketL)
                LOAD		R1		[GB + bucketL]	;
                CMP			R1		TRUE								;
                BNE			contBuck									;if not true, skip statement
                
                ;set bucketL to false
                LOAD		R1		FALSE								;
                STOR		R1		[GB + bucketL]	;

    contBuck :  ;update state
                LOAD  	R1 		RESET
             	  STOR  	R1  	[GB + currentState]
    
    						;update delay 
                LOAD		R1		BUCKET2_DELAY				;
                STOR		R1		[GB + delay]				;
                
                ;set timer_enable to true
                LOAD		R1		TRUE								;
                STOR		R1		[GB + timer_enable]	;
                
                ;skip else if statement(s)
                BRA 		nextSO

	elseTimeout2:	;else if (timeout2)
                LOAD    R1     [GB +  timeout2]
                CMP     R1     TRUE
                BNE     nextSO
                
                ;set timeout2 to false               
                LOAD	R1	FALSE										;
                STOR	R1	[GB + timeout2]					;
                
                ;update state
                LOAD  	R1 		INITIAL
             	  STOR  	R1  	[GB + currentState]
                
                ;display
                ;BRS 		display
                ;**FILL**
                
nextSO:					BRA		 main
              
;***************STATE RESET***************

reset: 
                ;display
                ;BRS 		display
                ;**FILL**
                
                ;set leds to false
                LOAD 		R1 		FALSE								;
                STOR		R1		[GB + led1]			; outpet led1 = false
                STOR		R1		[GB + led2]			; output led2 = false

								;if (!bucketR && !bucketL)
                LOAD		R1		[GB + bucketL]	;
                LOAD		R2		[GB + bucketR]	;
                OR			R1		R2									; do logical and with both statements
                CMP			R1		R0									; if 0, so both are not true, then proceed
                BNE			nextIf										; and skip rest of statement
                
                ;if (timeout)
                LOAD		R1		[GB + timeout]			;
                CMP			R1		TRUE								;
                BNE			nextIf										; if not skip rest of statement
                
                ;set timeout and conveyor to false
                LOAD    R1    FALSE
                STOR		R1	  [GB + timeout]			
                STOR		R1		[GB + conveyor]	;
                
                ;update delay
                LOAD		R1		[GB + delay]				;
                STOR		R1		[GB + BUCKET_DELAY]	;
                
                ;set timer_enable to true
                LOAD    R1    TRUE 
                STOR    R1    [GB + timer_enable]
                
                ;if (whiteDisc)
                LOAD		R1		[GB + whiteDisc]		;
                CMP			R1		TRUE								; if(whiteDisc)
                BNE			else											;
                
                ;set bucketL to true
                LOAD		R1 		TRUE
                STOR    R1    [GB + bucketL]	;
                BRA			nextIf 								;skip else statement

else : 					;set bucketR to true
								LOAD    R1    TRUE
								STOR		R1 		[GB + bucketR]		
									

            
; if (!bucketBtn && timeout)
    nextIf : 		LOAD		R1		[GB + buckBtn]		;
                LOAD		R2		[GB + timeout]			;
                CMP			R1		FALSE								;
                BNE			elseBuckTime							; if not false skip rest of if statement
                CMP			R2		TRUE								;
                BNE			elseBuckTime							; if not true skip rest of if statement 

                ;set timeout to false
                LOAD    R1    FALSE
                STOR		R1		[GB + timeout]			

                ;if (bucketR)
                LOAD		R1		[GB + bucketR]			;
                CMP			R1		TRUE								;
                BNE			elseBuckL									; if not true skip to next else if. 

                ;set bucketR to false
                LOAD    R1    FALSE
                STOR		R1   	[GB + bucketR]

                ;countBlack++
                LOAD		R1		[GB + countBlack]		; load countBlack
                ADD			R1		1										; add 1 to it
                STOR		R1		[GB + countBlack]		;  
                BRA     nextRE
            
    elseBuckL : ;if (bucketL)
                LOAD		R1		[GB + bucketL]			;
                CMP			R1		TRUE								;
                BNE			nextRE									  ; if not true skip to next else if. 

                ;set bucketL to false
                LOAD    R1    FALSE
                STOR		R1   	[GB + bucketL]           

                ;countWhite++
                LOAD		R1		[GB + countWhite]		;
                ADD			R1		1										;
                STOR		R1		[GB + countWhite]		;

    nextRE:			;update state
                LOAD  	R1 		PUSHER
                STOR  	R1  	[GB + currentState]

                ; else if (buckBtn && timeout)
 elseBuckTime : LOAD		R1	  [GB + buckBtn]		 ;
                LOAD		R2		[GB + timeout]			;
                AND			R1		R2									; take binary and
                CMP			R1		TRUE								; check if both hold
                BNE			nextRE2										; skip rest of statement
                
                ;update state
                LOAD    R1    FALSE
                STOR		R1  	[GB + timeout]			
                
                ;display
                ;BRS 		display
                ;**FILL**
                
 nextRE2:       BRA main

;***************STATE PAUSE***************                
pause:

              ;display update?


              ;disable outputs
              BRS   disableOutputs
	      			
              BRA 	main;
              
				      ;if (startPause && !pressed) {
              LOAD  R1  [GB + startPause]
              CMP   R1  TRUE
              BNE   nextPA2										;if not true, ignore if
              LOAD  R2  [GB + pressed]
              CMP   R2  FALSE
              BNE   nextPA2										;if not false, ignore if	

              ;set pressed and timerP_enable to true
              LOAD  R1  TRUE
              STOR  R1  [GB + pressed]
              STOR  R1  [GB + timerP_enable]
	      
nextPA1:      ; while (startPause && !timeout)
	      			LOAD  R1  [GB + startPause]
              LOAD  R2  [GB + timeout]
              CMP   R1  TRUE			
              BNE   nextPA2										;If true, get out of while
              CMP   R2  FALSE
              BNE   nextPA2										;If false, get out of while

							BRA   nextPA1										;finish loop

nextPA2:      ;currentState = prevState
              LOAD  R1  [GB + prevState]
              STOR  R1  [GB + currentState]	 

@END
      
      
      
      
      
			

      
      
      
      
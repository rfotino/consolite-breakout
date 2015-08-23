bootloader:
        MOVI SP stack           ; Initialize the stack pointer.
        CALL draw_walls
        CALL init
        JMPI main               ; Start the main loop

paddle_x:                       ; The (variable) x position of the paddle.
        0x0070
ball_x:                         ; The x position of the ball in 8.8 fixed
        0x7e00                  ; point.
ball_x_prev:
        0x0000
ball_y:                         ; The y position of the ball in 8.8 fixed
        0xb300                  ; point.
ball_y_prev:
        0x0000
score:                          ; The current score
        0x0000
num_bitmaps:                    ; Bitmaps for drawing 0-9
        0x7b6f 0x2c97 0x73e7 0x72cf 0x5bc9 0x79cf 0x79ef 0x7249 0x7bef 0x7bcf
is_running:                     ; If the game is currently running or
        0x0000                  ; if it is waiting for input.

init:                           ; Initializes the game
        PUSH FP
        PUSH A
        MOV FP SP

        MOVI A 0x0              ; Set the score to zero.
        STORI A score
        CALL draw_bricks        ; Draw all of the bricks.

        MOV SP FP
        POP A
        POP FP
        RET

main:                           ; Main loop, called 60 times per second
        TIMERST                 ; Resets the timer
        LOADI A ball_x          ; Set the previous ball position equal
        STORI A ball_x_prev     ; to the current ball position.
        LOADI A ball_y
        STORI A ball_y_prev
        CALL move_paddle        ; Move the paddle according to input.
        LOADI A is_running      ; Check if the game is running, if so
        MOVI L 0x1              ; do stuff like collision and scoring.
        CMP A L
        JEQ main_running
        CALL move_ball_with_paddle
        MOVI A 0x0              ; Check if the user has hit the space
        INPUT A A               ; bar, and if so begin the game.
        CMP A L
        JNE main_done
        STORI L is_running      ; Set is_running true
        CALL init               ; Initialize the game state
main_running:
        ;; TODO: implement what happens when the game is running
main_done:
        CALL draw_paddle
        CALL draw_ball
        CALL draw_score
        CALL wait               ; Wait until the 16ms for this iteration
        JMPI main               ; have elapsed, then restart.

wait:                           ; Waits until the time is 16 ms after
                                ; the last call to TIMERST.
        PUSH FP
        PUSH A
        PUSH B
        MOV FP SP

        MOVI A 0x10
wait_again:     
        TIME B
        CMP B A
        JB wait_again

        MOV SP FP
        POP B
        POP A
        POP FP
        RET

move_paddle:                    ; Moves the paddle to the left or the
        PUSH FP                 ; right or not at all depending on user
        PUSH A                  ; input.
        PUSH B
        PUSH C
        PUSH D
        PUSH E
        PUSH F
        PUSH L
        MOV FP SP

        MOVI L 0x0              ; Set the color to black.
        COLOR L

        MOVI B 0xb8             ; The y coord of the paddle.
        MOVI C 0x3              ; Speed at which the paddle moves
        MOVI D 0x5              ; Height of the paddle

        MOVI L 0x1
        MOVI E 0x1
        INPUT F E
        CMP F L
        JNE move_paddle_right
move_paddle_left:
        LOADI A paddle_x
        MOVI L 0x10             ; Make sure the paddle x position is
        CMP A L                 ; greater than the wall position.
        JBE move_paddle_done
        SUB A C
        STORI A paddle_x
        MOVI L 0x20
        ADD A L
        CALL draw_rectangle
        JMPI move_paddle_done
move_paddle_right:
        MOVI E 0x2
        INPUT F E
        CMP F L
        JNE move_paddle_done
        LOADI A paddle_x
        MOVI L 0xd0             ; Make sure the paddle x position is
        CMP A L                 ; less than the wall position minus
        JAE move_paddle_done    ; the paddle width.
        ADD A C
        STORI A paddle_x
        SUB A C
        CALL draw_rectangle
move_paddle_done:
        MOV SP FP
        POP L
        POP F
        POP E
        POP D
        POP C
        POP B
        POP A
        POP FP
        RET

move_ball_with_paddle:
        PUSH FP
        PUSH A
        PUSH B
        MOV FP SP

        LOADI A paddle_x        ; Set the ball x equal to the paddle
        MOVI B 0xe              ; x plus 14 (to center it).
        ADD A B
        MOVI B 0x8              ; Shift the ball x left by 8 because
        SHL A B                 ; it is in 8.8 fixed point.
        STORI A ball_x

        MOV SP FP
        POP B
        POP A
        POP FP
        RET

draw_walls:                     ; Draws a white 16px border on the top, left,
        PUSH FP
        PUSH A                  ; and right.
        PUSH B
        PUSH C
        PUSH D
        MOV FP SP
        
        MOVI A 0xff             ; Set the color to white
        COLOR A
        
        MOVI A 0x0              ; Draws a 256x16 rectangle at (0, 0)
        MOVI B 0x0
        MOVI C 0x100
        MOVI D 0x10
        CALL draw_rectangle

        MOVI B 0x10             ; Draws a 16x176 rectangle at (0, 16)
        MOVI C 0x10
        MOVI D 0xb0
        CALL draw_rectangle

        MOVI A 0xf0             ; Draws a 16x176 rectangle at (240, 16)
        CALL draw_rectangle

        MOV SP FP
        POP D
        POP C
        POP B
        POP A
        POP FP
        RET

draw_bricks:
        PUSH FP
        PUSH A
        PUSH B
        PUSH C
        PUSH D
        PUSH E
        PUSH F
        PUSH G
        PUSH H
        PUSH I
        PUSH L
        MOV FP SP

        MOVI A 0x10             ; Starting x position of brick
        MOVI B 0x20             ; Starting y position of brick
        MOVI C 0x1b             ; Width of brick is 27 pixels
        MOVI D 0x7              ; Height of brick is 7 pixels
        MOVI E 0x0              ; x counter
        MOVI F 0x8              ; x limit
        MOVI G 0x0              ; y counter
        MOVI H 0x6              ; y limit
        MOVI L 0x1              ; Used to increment by 1

        MOVI I 0x3              ; Set the initial brick color to blue
        COLOR I
        
draw_bricks_next:
        CALL draw_rectangle
        ADD A C
        ADD A L                 ; Add 1px gap
        ADD E L
        CMP E F
        JB draw_bricks_next
        MOVI A 0x10
        MOVI E 0x0
        ADD B D
        ADD B L                 ; Add 1px gap
        ADD G L
        MOVI H 0x2              ; If the row is less than 2, it is
        CMP G H                 ; still blue so we don't need to change
        JB draw_bricks_next     ; the color.
        MOVI I 0xec             ; Else set the color to orange. If the
        COLOR I                 ; row is less than 4, draw the next
        MOVI H 0x4              ; row in orange.
        CMP G H
        JB draw_bricks_next
        MOVI I 0x1c             ; Else set the color to green. If the
        COLOR I                 ; row is less than 6, draw the next
        MOVI H 0x6              ; row in blue. Otherwise we're done
        CMP G H                 ; drawing bricks.
        JB draw_bricks_next

        MOV SP FP
        POP L
        POP I
        POP H
        POP G
        POP F
        POP E
        POP D
        POP C
        POP B
        POP A
        POP FP
        RET
        
draw_ball:
        PUSH FP
        PUSH A
        PUSH B
        PUSH C
        PUSH D
        PUSH L
        MOV FP SP

        MOVI C 0x4              ; The width and height of the ball.
        MOVI D 0x4

        LOADI A ball_x
        LOADI B ball_x_prev
        CMP A B
        JNE draw_ball_black
        LOADI A ball_y
        LOADI B ball_y_prev
        CMP A B
        JNE draw_ball_black
        JMPI draw_ball_white

draw_ball_black:
        MOVI L 0x0              ; Set the color to black
        COLOR L
        LOADI A ball_x_prev     ; Draw the ball at the prev x and y coords
        LOADI B ball_y_prev     ; in memory.
        MOVI L 0x8              ; The x and y coords are in 8.8 fixed
        SHRL A L                ; point, so we have to shift them right
        SHRL B L                ; to get the integer values.
        CALL draw_rectangle

draw_ball_white:
        MOVI L 0xff             ; Set the color to white
        COLOR L
        LOADI A ball_x          ; Draw teh ball at the current x and
        LOADI B ball_y          ; y coords in memory.
        MOVI L 0x8
        SHRL A L
        SHRL B L
        CALL draw_rectangle

draw_ball_done:
        MOV SP FP
        POP L
        POP D
        POP C
        POP B
        POP A
        POP FP
        RET
                
draw_paddle:
        PUSH FP
        PUSH A
        PUSH B
        PUSH C
        PUSH D
        MOV FP SP

        MOVI A 0xff             ; Set the color to white
        COLOR A
        
        LOADI A paddle_x        ; Draw the paddle at the x position
        MOVI B 0xb8             ; stored in memory.
        MOVI C 0x20
        MOVI D 0x5
        CALL draw_rectangle

        MOV SP FP
        POP D
        POP C
        POP B
        POP A
        POP FP
        RET
        
draw_score:
        PUSH FP
        PUSH A
        PUSH B
        PUSH C
        PUSH E
        PUSH F
        PUSH L
        PUSH M
        PUSH N
        MOV FP SP

        MOVI L 0x0              ; Set the color to black
        COLOR L

        LOADI E score           ; Load the score
        MOVI M 0x0              ; Counter for drawing digits
        MOVI N 0x4              ; Number of digits
        MOVI A 0x28             ; x position start
        MOVI B 0x3              ; y position

draw_score_digit:
        MOV F E
        MOVI L 0xa
        DIV F L
        MUL F L
        MOV C E
        SUB C F
        DIV E L
        CALL draw_number
        MOVI L 0x8
        SUB A L
        MOVI L 0x1
        ADD M L
        CMP M N
        JB draw_score_digit

        MOV SP FP
        POP N
        POP M
        POP L
        POP F
        POP E
        POP C
        POP B
        POP A
        POP FP
        RET

draw_number:                    ; Draws the number C to (A, B)
        PUSH FP
        PUSH C
        PUSH D
        PUSH E
        PUSH F
        PUSH G
        PUSH H
        PUSH L
        PUSH M
        MOV FP SP
        
        MOVI M num_bitmaps
        MOVI L 0x1              ; C is the number argument 0-9.
        SHL C L                 ; We shift it because each bitmap is
        ADD M C                 ; two bytes.
        LOAD M M                ; Now the bitmap is stored in D

        MOVI L 0x6              ; Width of bitmap
        MOV E A                 ; E is the x counter
        MOV F A                 ; F is the x limit
        ADD F L

        MOVI L 0xa              ; Height of bitmap
        MOV G B                 ; G is the y counter
        MOV H B                 ; H is the y limit
        ADD H L

        MOVI C 0x2              ; Width of bitmap pixel
        MOVI D 0x2              ; Height of bitmap pixel

draw_number_pixel:
        MOVI L 0x1
        SHL M L                 ; Get the next bit in bitmap
        MOVI L 0x8000           ; Only the high bit set
        TST M L                 ; Check if the high bit is set
        JEQ draw_number_pixel_done ; If not, skip over drawing
        PUSH A
        PUSH B
        MOV A E
        MOV B G
        CALL draw_rectangle
        POP B
        POP A
draw_number_pixel_done:
        ADD E C
        CMP E F
        JB draw_number_pixel
        MOV E A
        ADD G D
        CMP G H
        JB draw_number_pixel

        MOV SP FP
        POP M
        POP L
        POP H
        POP G
        POP F
        POP E
        POP D
        POP C
        POP FP
        RET

draw_rectangle:                 ; Draws rectangle at (A, B) to (A+C, B+D).
        PUSH FP
        PUSH E                  ; Save all the registers we will use
        PUSH F                  ; so that we can restore them later.
        PUSH G
        PUSH H
        PUSH L
        MOV FP SP

        MOVI L 0x1              ; Used to increment by 1
        MOV E A                 ; X-coordinate counter
        MOV F B                 ; Y-coordinate counter
        MOV G E                 ; G is (x + width), the x-coord limit
        ADD G C
        MOV H F                 ; H is (y + height), the y-coord limit
        ADD H D

draw_rectangle_pixel:           ; Label to jump to if we still want to
        PIXEL E F               ; draw the next pixel.
        ADD E L                 ; Increment the x-coordinate counter
        CMP E G                 ; If x-counter < x + width, continue
        JB draw_rectangle_pixel ; drawing
        ADD F L                 ; Else increment the y-coordinate
        MOV E A                  ; and set the x-coord counter to x-start
        CMP F H                 ; If y-counter < y + height, continue
        JB draw_rectangle_pixel ; drawing

        MOV SP FP
        POP L                   ; Restore the registers we used in
        POP H                   ; reverse order.
        POP G
        POP F
        POP E
        POP FP
        
        RET                     ; Jump back to the saved location of IP

stack:                          ; The location of the stack

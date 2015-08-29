; Breakout in Consolite Assembly
; Written by Robert Fotino

bootloader:
        MOVI SP stack           ; Initialize the stack pointer.
        CALL draw_walls
        CALL init
        JMPI main               ; Start the main loop

bricks_blue:
        0xffff
bricks_orange:
        0xffff
bricks_green:
        0xffff
paddle_x:                       ; The (variable) x position of the paddle.
        0x0070
ball_x:                         ; The position of the ball is in 8.8 fixed point
        0x7e00                  ; format.
ball_x_prev:
        0x0000
ball_y:
        0xb300
ball_y_prev:
        0x0000
ball_speed:                     ; The ball's speed is multiplied by its
        0x0000                  ; direction to get the velocity. Speed is in
                                ; 15.1 fixed point format.
ball_dir_x:                     ; The ball's direction is a unit vector in
        0x0000                  ; 1.7.8 fixed point format.
ball_dir_y:
        0x0000
                                ; Precalculated directions for -60 through 60
                                ; degrees, at 15 degree intervals. Numbers in
lookup_dir_x:                   ; 1.7.8 fixed point format.
        0x80dd 0x80b5 0x8080 0x8042 0x8009 0x0042 0x0080 0x00b5 0x00dd
lookup_dir_y:
        0x8080 0x80b5 0x80dd 0x80f7 0x8100 0x80f7 0x80dd 0x80b5 0x8080
score:                          ; The current score
        0x0000
score_prev:                     ; The previous score
        0x0000
num_bitmaps:                    ; Bitmaps for drawing 0-9
        0x7b6f 0x2c97 0x73e7 0x72cf 0x5bc9 0x79cf 0x79ef 0x7249 0x7bef 0x7bcf
is_running:                     ; If the game is currently running or
        0x0000                  ; if it is waiting for input.

init:                           ; Initializes the game
        PUSH FP
        PUSH A
        PUSH B
        MOV FP SP

        MOVI A 0x0              ; Set the score to zero.
        STORI A score
        MOVI A 0xffff           ; Set the bricks to all alive.
        STORI A bricks_blue
        STORI A bricks_orange
        STORI A bricks_green
        CALL draw_bricks        ; Draw all of the bricks.

        RND A                   ; Get a random number between 0 and 8 to index
        MOVI B 0xff             ; the directions lookup table to load
        AND A B
        MOVI B 0x9
        MUL A B
        MOVI B 0x100
        DIV A B
        CALL change_direction

        MOVI A 0x4              ; Initialize the ball's speed to integer 2.
        STORI A ball_speed

        MOV SP FP
        POP B
        POP A
        POP FP
        RET

main:                           ; Main loop, called 60 times per second
        TIMERST                 ; Resets the timer
        LOADI A ball_x          ; Set the previous ball position equal
        STORI A ball_x_prev     ; to the current ball position.
        LOADI A ball_y
        STORI A ball_y_prev
        LOADI A score           ; Set the previous score equal to the current
        STORI A score_prev      ; score.
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
        CALL move_ball
        CALL collide_walls
        CALL collide_paddle
        CALL collide_bricks_all
        CALL check_endgame
main_done:
        CALL draw_ball
        CALL draw_paddle
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

move_ball:
        PUSH FP
        PUSH A
        PUSH B
        PUSH C
        PUSH D
        PUSH E
        PUSH L
        PUSH M
        PUSH N
        MOV FP SP

        LOADI A ball_dir_x
        LOADI B ball_dir_y
        LOADI C ball_speed
        LOADI M ball_x
        LOADI N ball_y

        MOVI L 0xf              ; Get the leading bits for the direction, used
        MOV D A                 ; to determine the sign. Shift the direction by
        SHRL D L                ; 15 to get 0x1 for negative or 0x0 for positive
        MOV E B
        SHRL E L

        MOVI L 0x7fff           ; Get rid of the sign bit from the direction
        AND A L                 ; so that we can do math with it.
        AND B L

        MUL A C                 ; Multiply the speed by the direction to get
        MUL B C                 ; the velocity to add to / sub from position.
        MOVI L 0x1              ; The velocity must be shifted right by one
        SHRL A L                ; because the speed is in 15.1 fixed point
        SHRL B L                ; format.

        MOVI L 0x1              ; Add or subtract the x and y velocities to or
        CMP D L                 ; from the ball's position, to get its new
        JEQ move_ball_x_neg     ; position.
        ADD M A
        JMPI move_ball_x_done
move_ball_x_neg:
        SUB M A
move_ball_x_done:
        CMP E L
        JEQ move_ball_y_neg
        ADD N B
        JMPI move_ball_y_done
move_ball_y_neg:
        SUB N B
move_ball_y_done:
        STORI M ball_x
        STORI N ball_y

        MOV SP FP
        POP N
        POP M
        POP L
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

        MOVI A 0xb300           ; Set the ball y equal to the paddle y minus
        STORI A ball_y          ; 5 (to be floating 1 pixel above the paddle).

        MOV SP FP
        POP B
        POP A
        POP FP
        RET

change_direction:
        PUSH FP
        PUSH B
        PUSH C
        PUSH L
        MOV FP SP

        MOVI L 0x1
        MOV C A
        SHL C L

        MOVI B lookup_dir_x
        ADD B C
        LOAD B B
        STORI B ball_dir_x

        MOVI B lookup_dir_y
        ADD B C
        LOAD B B
        STORI B ball_dir_y

        MOV SP FP
        POP L
        POP C
        POP B
        POP FP
        RET

collide_walls:
        PUSH FP
        PUSH A
        PUSH B
        MOV FP SP

        LOADI A ball_x          ; Check for collision with the left wall. If
        MOVI B 0x1000           ; there is collision, reverse the x direction
        CMP A B                 ; of the ball and set its position to the
        JA collide_walls_right  ; minimum x position.
        MOV A B
        STORI A ball_x
        LOADI A ball_dir_x
        MOVI B 0x7fff
        AND A B
        STORI A ball_dir_x
        JMPI collide_walls_top
collide_walls_right:            ; Check for collision with the right wall.
        MOVI B 0xec00
        CMP A B
        JB collide_walls_top
        MOV A B
        STORI A ball_x
        LOADI A ball_dir_x
        MOVI B 0x8000
        OR A B
        STORI A ball_dir_x
collide_walls_top:
        LOADI A ball_y          ; Check for collision with the top wall.
        MOVI B 0x1000
        CMP A B
        JA collide_walls_done
        MOV A B
        STORI A ball_y
        LOADI A ball_dir_y
        MOVI B 0x7fff
        AND A B
        STORI A ball_dir_y
collide_walls_done:

        MOV SP FP
        POP B
        POP A
        POP FP
        RET

collide_paddle:
        PUSH FP
        PUSH A
        PUSH B
        PUSH C
        PUSH D
        PUSH E
        PUSH L
        MOV FP SP

        LOADI A paddle_x
        LOADI B ball_x
        LOADI C ball_y
        LOADI D ball_x_prev
        LOADI E ball_y_prev
        MOVI L 0x8              ; Turn the 8.8 position into an integer value
        SHRL B L                ; by shifting it 8 to the right.
        SHRL C L
        SHRL D L
        SHRL E L

        MOVI L 0x4              ; Check if the ball is to the left of the
        MOV F B                 ; leftmost part of the paddle, meaning no
        ADD F L                 ; collision.
        CMP F A
        JB collide_paddle_left
        MOVI L 0x20             ; Check if the ball is to the right of the
        MOV F A                 ; rightmost part of the paddle, meaning no
        ADD F L                 ; collision.
        CMP B F
        JAE collide_paddle_left
        MOVI F 0xb4             ; Check if the ball is above the paddle's
        CMP C F                 ; surface, meaning no collision.
        JB collide_paddle_left
        CMP E F                 ; Check if the ball's previous position was
        JAE collide_paddle_left ; below the paddle, meaning no collision.

        MOVI L 0xb400           ; Set the ball's y position to the maximum
        STORI L ball_y          ; and set it up with a new velocity.
        SUB B A
        MOVI L 0x4
        ADD B L
        MOVI L 0x2
        SHRL B L
        MOV A B
        CALL change_direction
        JMPI collide_paddle_done

collide_paddle_left:            ; Now we check for horizontal collision
        MOVI L 0xb4             ; Check that ball_y + ball_height < paddle_y,
        CMP C L                 ; meaning no collision.
        JB collide_paddle_done
        MOVI L 0xbd             ; Check that ball_y > paddle_y + paddle_height,
        CMP C L                 ; meaning no collision.
        JA collide_paddle_done
        MOVI L 0x4
        MOV F D                 ; Check ball_x_prev + ball_width > paddle_x,
        ADD F L                 ; meaning no left collision.
        CMP F A
        JA collide_paddle_right
        MOV F B                 ; Check that ball_x + ball_width < paddle_x,
        ADD F L                 ; meaning no left collision.
        CMP F A
        JB collide_paddle_right
        MOV F A                 ; We had collision, set the ball in its correct
        MOVI L 0x4              ; position.
        SUB F L
        MOVI L 0x8
        SHL F L
        STORI F ball_x
        LOADI F ball_dir_x      ; Update the direction.
        MOVI L 0x8000
        OR F L
        STORI F ball_dir_x
        JMPI collide_paddle_done
collide_paddle_right:
        MOVI L 0x20
        MOV F A                 ; Check ball_x_prev < paddle_x + paddle_width,
        ADD F L                 ; meaning no collision.
        CMP F D
        JA collide_paddle_done
        CMP F B                 ; Check ball_x > brick_x + brick_width,
        JB collide_paddle_done   ; meaning no collision.
        MOVI L 0x8              ; We had collision, set the ball in its correct
        SHL F L                 ; position.
        STORI F ball_x
        LOADI F ball_dir_x      ; Update the direction.
        MOVI L 0x7fff
        AND F L
        STORI F ball_dir_x

collide_paddle_done:
        MOV SP FP
        POP L
        POP E
        POP D
        POP C
        POP B
        POP A
        POP FP
        RET

collide_bricks_all:
        PUSH FP
        PUSH A
        PUSH B
        PUSH C
        PUSH D
        MOV FP SP

        LOADI A bricks_blue
        MOVI B 0x20             ; Starting y position.
        MOVI C 0x64             ; Point value if collided, 100.
        MOVI D 0x7              ; Min speed if collided.
        CALL collide_bricks_line
        STORI A bricks_blue

        LOADI A bricks_orange
        MOVI B 0x30             ; Starting y position.
        MOVI C 0x32             ; Point value if collided, 50.
        MOVI D 0x5              ; Min speed if collided.
        CALL collide_bricks_line
        STORI A bricks_orange

        LOADI A bricks_green
        MOVI B 0x40             ; Starting y position.
        MOVI C 0x19             ; Point value if collided, 25.
        MOVI D 0x4              ; Min speed if collided.
        CALL collide_bricks_line
        STORI A bricks_green

        MOV SP FP
        POP D
        POP C
        POP B
        POP A
        POP FP
        RET

collide_bricks_line:            ; A is brick vector, B is y position, C is point
                                ; value of bricks, D is min speed if collided.
        PUSH FP
        PUSH E
        PUSH F
        PUSH G
        PUSH H
        PUSH L
        PUSH M
        PUSH N
        MOV FP SP

        MOVI E 0x10             ; Starting x position
        MOVI F 0xf0             ; Maximum x position
        MOV G B
        MOVI L 0x10
        ADD G L                 ; Maximum y position
        MOVI H 0x8000           ; The bit we're testing for

collide_bricks_line_next:
        TST A H                 ; Check if the bit we're testing is set.
        JEQ collide_bricks_line_inc ; If the bit is a zero, don't do collide.
        MOV L A                 ; Save the value of A in L, and set up A to
        MOV A E                 ; be the x position for calling collide_brick.
        CALL collide_brick      ; This sets A to be 1 if there was a collision
        MOV M A                 ; Set return value in M
        MOV A L                 ; Restore A
        MOVI N 0x1              ; Check if the return value was 1, if it wasn't
        CMP M N                 ; then don't do anything.
        JNE collide_bricks_line_inc
        LOADI L score           ; Update the score
        ADD L C
        STORI L score
        XOR A H                 ; Set the bit for this brick to 0.
        LOADI L ball_speed      ; Increase the ball speed if it is not at the
        CMP L D                 ; minimum.
        JAE collide_bricks_line_inc
        STORI D ball_speed
collide_bricks_line_inc:
        MOVI L 0x1              ; Shift the bit we're testing
        SHRL H L
        MOVI L 0x1c             ; Increment the x position, check if it's
        ADD E L                 ; greater than the maximum.
        CMP E F
        JB collide_bricks_line_next
        MOVI E 0x10
        MOVI L 0x8
        ADD B L
        CMP B G
        JB collide_bricks_line_next

        MOV SP FP
        POP N
        POP M
        POP L
        POP H
        POP G
        POP F
        POP E
        POP FP
        RET

collide_brick:                  ; Check for collision with a single brick.
                                ; A is the x position, B is the y position.
                                ; If there is a collision, this function will
                                ; handle updating the ball's position and
                                ; direction and will black out the brick.
        PUSH FP
        PUSH C
        PUSH D
        PUSH E
        PUSH F
        PUSH G
        PUSH H
        PUSH I
        PUSH L
        PUSH N
        MOV FP SP

        MOVI N 0x0              ; Return value

        MOVI C 0x1c             ; Brick width
        MOVI D 0x8              ; Brick height

        LOADI E ball_x          ; Get the ball x and y and shift them to their
        LOADI F ball_y          ; integer values.
        LOADI G ball_x_prev
        LOADI H ball_y_prev
        MOVI L 0x8
        SHRL E L
        SHRL F L
        SHRL G L
        SHRL H L

        MOVI L 0x4              ; Check that ball_x + ball_width < brick_x,
        MOV I E                 ; in which case there is no vertical collision.
        ADD I L
        CMP I A
        JB collide_brick_vert_done
        MOV I A                 ; Check that ball_x > brick_x + brick_width,
        ADD I C                 ; in which case there is no vertical collision.
        CMP E I
        JA collide_brick_vert_done
        MOV I F                 ; Check that ball_y + ball_height > brick_y,
        ADD I L                 ; meaning no top collision
        CMP I B
        JB collide_brick_top_done
        MOV I H                 ; Check ball_y_prev + ball_height < brick_y,
        ADD I L                 ; meaning no top collision
        CMP I B
        JA collide_brick_top_done
        MOV I B                 ; We had collision, set the ball in its correct
        MOVI L 0x4              ; position.
        SUB I L
        MOVI L 0x8
        SHL I L
        STORI I ball_y
        LOADI I ball_dir_y      ; Update the direction.
        MOVI L 0x8000
        OR I L
        STORI I ball_dir_y
        MOVI L 0x0              ; Black out the brick
        COLOR L
        CALL draw_rectangle
        MOVI N 0x1              ; Set the return value
        JMPI collide_brick_done
collide_brick_top_done:
        MOV I B                 ; Check that ball_y > brick_y + brick_height,
        ADD I D                 ; meaning no collision.
        CMP I F
        JB collide_brick_vert_done
        CMP I H                 ; Check ball_y_prev < brick_y + brick_height
        JA collide_brick_vert_done
        MOV I B                 ; We had collision, set the ball in its correct
        ADD I D                 ; position.
        MOVI L 0x8
        SHL I L
        STORI I ball_y
        LOADI I ball_dir_y      ; Update the direction.
        MOVI L 0x7fff
        AND I L
        STORI I ball_dir_y
        MOVI L 0x0              ; Black out the brick
        COLOR L
        CALL draw_rectangle
        MOVI N 0x1              ; Set the return value
        JMPI collide_brick_done
collide_brick_vert_done:
        MOV I F                 ; Check that ball_y + ball_height < brick_y,
        MOVI L 0x4              ; meaning no collision.
        ADD I L
        CMP I B
        JB collide_brick_done
        MOV I B                 ; Check that ball_y > brick_y + brick_height,
        ADD I D                 ; meaning no collision.
        CMP F I
        JA collide_brick_done
        MOV I G                 ; Check ball_x_prev + ball_width > brick_x,
        ADD I L                 ; meaning no left collision.
        CMP I A
        JA collide_brick_left_done
        MOV I E                 ; Check that ball_x + ball_width < brick_x,
        ADD I L                 ; meaning no left collision.
        CMP I A
        JB collide_brick_left_done
        MOV I A                 ; We had collision, set the ball in its correct
        MOVI L 0x4              ; position.
        SUB I L
        MOVI L 0x8
        SHL I L
        STORI I ball_x
        LOADI I ball_dir_x      ; Update the direction.
        MOVI L 0x8000
        OR I L
        STORI I ball_dir_x
        MOVI L 0x0              ; Black out the brick
        COLOR L
        CALL draw_rectangle
        MOVI N 0x1              ; Set the return value
        JMPI collide_brick_done
collide_brick_left_done:
        MOV I A                 ; Check ball_x_prev < brick_x + brick_width,
        ADD I C                 ; meaning no collision.
        CMP I G
        JA collide_brick_done
        CMP I E                 ; Check ball_x > brick_x + brick_width,
        JB collide_brick_done   ; meaning no collision.
        MOVI L 0x8              ; We had collision, set the ball in its correct
        SHL I L                 ; position.
        STORI I ball_x
        LOADI I ball_dir_x      ; Update the direction.
        MOVI L 0x7fff
        AND I L
        STORI I ball_dir_x
        MOVI L 0x0              ; Black out the brick
        COLOR L
        CALL draw_rectangle
        MOVI N 0x1              ; Set the return value
        JMPI collide_brick_done
collide_brick_done:
        MOV A N                 ; We stored the return value in N temporarily,
                                ; set it back to A.
        MOV SP FP
        POP N
        POP L
        POP I
        POP H
        POP G
        POP F
        POP E
        POP D
        POP C
        POP FP
        RET

check_endgame:
        PUSH FP
        PUSH A
        PUSH B
        MOV FP SP

        LOADI A ball_y
        MOVI B 0xf000
        CMP A B
        JBE check_endgame_done
        MOVI A 0x0
        STORI A is_running

check_endgame_done:
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

        LOADI E score            ; Load the score and previous score.
        LOADI F score_prev
        CMP E F
        JEQ draw_score_blank_done
        MOVI L 0xff             ; If the current score is different than the
        COLOR L                 ; previous score, first clear the score rect.
        MOVI A 0x10
        MOVI B 0x3
        MOVI C 0x1e
        MOVI D 0xa
        CALL draw_rectangle
draw_score_blank_done:

        MOVI L 0x0              ; Set the color to black
        COLOR L

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

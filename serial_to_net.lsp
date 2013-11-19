;;程序设计是没有退出的，所以函数的返回要及时 ,函数应该只考虑正确情况, openwrt BARRIER BREAKER (Bleeding Edge, r34253)
;;当newlisp退出时，所有没有close的open 的handle都会被 close掉
;;主要是openwrt下面使用的
(constant 'PORT1 8085)
(constant 'PORT2 8086)
(constant 'ReadMax 138)
(constant 'SIGINT 2)
(constant 'DEV0 "/dev/ttyUSB0")
(constant 'DEV1 "/dev/ttyUSB1")

(define (get-parent_id) (sys-info 6))
(define (getpid) (sys-info 7))

(define (assert)
	(setq ret  "") (doargs (i) (extend ret (string i))) 
	(write 1 ret ) ;; 强制显示在stdout上
)

(setq children_num 3);; how many process forked

(set 'talk (share))     (share talk 0)
(set 'content (share))  (share content "")

(if (or (= ostype "Win32") (= ostype "Cygwin")) ;;设定设备名称，至于S10，也未必一定是S10,具体视cygwin环境
	(setq dev "/dev/ttyS10") ;; on win32 with cygwin!
	(setq dev DEV0) ;; all linux platform,espcially in openwrt 
)

(setq dev1 DEV1);; 这儿没有考虑win了


(define (strtohex str);;xx xx xx to hex
	(setq bn (length (parse str " ")))
	(setq bstr (chop (dup "b" bn) 1) )
	(setq ret nil)
	
	(dolist (x (parse str " "))
		(extend ret (pack "b" (int x 0 16)) )
	)
	ret
)

(define (make_serial_cmd cmd str)
	(if-not (nil? str)
		(begin
			(cond
				((= (trim cmd ) "AT+PING")
					(if (string? str)
						(setq cmd_line (string "d8 " (slice content 0 2) " " (slice content  2 4) " 70 69 6e 67"))
					)
				)
				((= (trim cmd ) "AT+SD") ;;短地址
					(if (string? str)
						(setq cmd_line (string "d8 " (slice content 0 2) " " (slice content 2 4) " 02"))
					)
				)
				((= (trim cmd) "AT&PWM");; pwm wave; xx xx, 14 r g b a 
					(if (and (= (length str) 2) (list? str)) 
						(begin
							(setq t1 (parse (str 0) " "))
							(setq t2 (parse (str 1) " "))
							(if (and (= (length t1) 2) (= (length t2) 4))
								(begin
									(setq cmd_line (string "d8 " (str 0) " 14 " (str 1) ) )
								)
							)
						)
					)
					(if (and (= (length str) 1) (list? str)) ;; pwm only for serial port | 14 r g b a
						(begin
							(setq t1 (parse (str 0) " "))
							(if (= (length t1 ) 4)
								(setq cmd_line (string "14 " (str 0)))
							)
						)
					)
					
				)
					
				((= (trim cmd) "AT&ROUTER")
					(setq cmd_line (string "e0"))
				)
				((= (trim cmd) "AT&COORDI")
					(setq cmd_line (string "c0"))
				)
				((= (trim cmd) "AT&PAN")
					(if (and (= (length str) 2) (list? str))
						(begin
							(if (and (= (length (parse (str 0) " ")) 2 ) (= (length (parse (str 1)  " ")) 2) )
								(begin
									(setq t1 (parse str " "))
									(setq t2 (parse str " "))
									(setq cmd_line (string "d8 " (t1 0) " " (t1 1) " 70 " (t2 0) " " (t2 1) ))
								)
							)
						)
					)
					(if (and (= (length str) 1) (list? str))
						(begin
							(setq t1 (parse (str 0 ) " "))
							(if (= (length t1) 2)
								(setq cmd_line (string "70 " (t1 0) " " (t1 1)))
							)
						)
					)
				)
				((= (trim cmd) "AT&SW") ;;for switch
					(if (and (= (length str) 2) (list? str)) ; xx xx,xx xx xx xx xx, xx xx xx xx xx | low , high
						(begin
							(if (and (= (length (parse (str 0) " ")) 5) (= (length (parse (str 1) " ")) 5))
								(setq cmd_line (list (str 0) (str 1)))
							) 
						)
					)
					(if (and (= (length str) 3) (list? str)) ;;xx xx,xx xx xx xx xx, xx xx xx xx xx
						(begin
							(setq t1 (parse (str 0) " "))
							(setq t2 (parse (str 1) " "))
							(setq t3 (parse (str 2) " "))
							(if (and (= (length t1) 2) (= (length t2) 5) (= (length t3) 5))
								(begin
									(setq cmd_line (list (string "d8 " (str 1)) (string "d8 " (str 2))))
								)
							)
						)
					)
				)

			)
		)
	)
	cmd_line
)

(define (parse_atcmd str)
	(setq ret nil)
	(cond 
		((starts-with str "AT+")
			(setq tmplst (parse str "="))
			(setq cmd (tmplst 0) )
			(if (> (length  tmplst) 1)
				(begin
					(setq content (tmplst 1))
				)
			)
			(setq ret (make_serial_cmd cmd content) )
		)
		( (starts-with str "AT&")
			(setq tmplst (parse str "="))
			(setq cmd (tmplst 0))
			(if (> (length tmplst)  1)
				(begin
					(setq content_lst (parse ( tmplst 1 ) ","))
				)
			)
			(setq ret (make_serial_cmd cmd content_lst) )
		)
	)
	
	ret
)



(define (set_serial dev_str)
	(setq baud 38400) ;;波特率
	(if (or (= ostype "Win32") (= ostype "Cygwin")) ;;串口配置指令
		(setq serial_port_setting (string "stty -F " dev_str " cs8 " baud " -ixon -icanon  min 0 time 30") )
		(setq serial_port_setting (string "stty -F " dev_str " cs8 " baud " ignbrk -brkint -icrnl -imaxbel -opost -onlcr -isig -icanon -iexten -echo -echoe -echok -echoctl -echoke noflsh   min 150 time 1"))
	)
	(exec serial_port_setting)
)

(define (write_serial query len )
		(if (file? dev)
			(begin
				(setq usbdev (open dev "w"))
				(setq nw (write usbdev query len))
				;(assert nw)
				(close usbdev)
			)
		)
)

(define (write_string_to_serial str)
    (setq str_lst (parse str " "))
	(setq len (length str_lst))
    (setq ret "")
    (dolist (x str_lst)
        (setq tmp_str (slice x 0 2)) ;; force the input to be XX
        (extend ret (char (int tmp_str 0 16)))
    )
    (write_serial ret len)
)


(define (tcp_8088) ;; tcp 进程,专门处理串口数据
	
	(set 'listen (net-listen 8088))
	
	(unless (not (nil? listen)) 
		(begin
			(print "listening 8088 failed\n")
			(abort)
			(exit)
		)
	)
	(print "Waiting for connection on: 8088 \n")

	(while (!= (share talk) 19)
		(set 'connection (net-accept listen))
		(while (not (net-select connection "r" 1000)))
		
		(while (net-select connection "w" 1000)
			(if (setq rvlen (net-receive connection buff ReadMax "\r\n"))
				(begin
					(share talk 1)
					(sleep 200)
					;(assert buff)
					(setq atret (parse_atcmd (chop buff 2)))
					(assert atret "\n")
					(cond 
						((list? atret)
							(dolist (x atret)
								(write_string_to_serial x)
								(sleep 100);;; sleep 100ms to serial  
							)
						)
						((string? atret)
							(write_string_to_serial atret)
						)
					)
					
					;; here deal with data
				)
			)
		)
		
		(assert "close connect " connection "\n")
		(net-close connection)
	)		
)

(define (tcp_8087) ;; tcp 进程,专门处理串口数据; 在这儿spawn了几个进程，共同处理任务
	
	(while (!= (share talk) 19)
		(set 'connection (net-accept server))
		(net-send connection ">" 1)
		(while (not (net-select connection "r" 1000)))
		
				(while (net-select connection "w" 1000)
					(if (setq rvlen (net-receive connection buff ReadMax "\r\n"))
						(begin
							(share talk 1)
							(sleep 200)
							;(assert buff)
							(setq atret (parse_atcmd (chop buff 2)))
							(assert atret "\n")
							(cond 
								((list? atret)
									(dolist (x atret)
										(write_string_to_serial x)
										(sleep 100);;; sleep 100ms to serial  
									)
								)
								((string? atret)
										(write_string_to_serial atret)
								)
							)
							
							(if (starts-with buff "AT+")
								(begin
									(dotimes (z 50 (= (share talk) 999)) (assert "sleep10\n") (sleep 30) );; sleep 约500ms就结束等,挺慢的，串口的反应
									(share talk 0);; 清除 标志，read process有返回了	
									(assert (share content) "\n")
									
									(net-send connection (share content))
								)
							)
							
							(if (starts-with buff "close")
								(net-close connection)
								(net-send connection ">" 1)
							)
							;; here deal with data
						)
					)
				)
			
		
		(assert "close connect 8087\n")
		(net-close connection)
	)		
)

(define (tcp_8084) ;; tcp 进程,这个主要是用来交互openwrt上的数据库之类，比如状态表，节点表，别名表;; telnet 用的
	(set 'listen (net-listen 8086))
	
	(unless (not (nil? listen)) 
		(begin
			(print "listening " PORT2 " failed\n")
			(abort)
			(exit)
		)
	)
	(print "Waiting for connection on: 8086 \n")

	(while (!= (share talk) 20)
		(set 'connection (net-accept listen))
		(net-send connection ">" 1)
		(while (not (net-select connection "r" 1000)))
		
		(while (net-select connection "w" 1000)
			(if (setq rvlen (net-receive connection buff ReadMax "\n"))
				(begin
					(assert buff) (assert "\n")
					;; here deal with data
					(if (= (chop buff 2) "close")
						(net-close connection)
						(net-send connection ">" 1)
					)
							(setq atret (parse_atcmd (chop buff )))
							(assert atret "\n")
							(cond 
								((list? atret)
									(dolist (x atret)
										(write_string_to_serial x)
										(sleep 100);;; sleep 100ms to serial  
									)
								)
								((string? atret)
										(write_string_to_serial atret)
								)
							)						
				)
			)
		)
		(assert "close connect " (net-peer connection) "->" (net-local connection) " \n")
		(net-close connection)
	)		
)

(define (tcp_8083) ;; tcp 进程,专门处理串口数据
	
	(set 'listen (net-listen 8085))
	
	(unless (not (nil? listen)) 
		(begin
			(print "listening " PORT1 " failed\n")
			(abort)
			(exit)
		)
	)
	(print "Waiting for connection on: 8085 \n")

	(while (!= (share talk) 19)
		(set 'connection (net-accept listen))
		(while (not (net-select connection "r" 1000)))
		
				(while (net-select connection "w" 1000)
					(if (setq rvlen (net-receive connection buff ReadMax "\r\n"))
						(begin
							(share talk 1)
							(sleep 200)
							;(assert buff)
							
							(write_string_to_serial (chop buff 2))
							
							(dotimes (z 50 (= (share talk) 999)) (assert "sleep10\n") (sleep 20) );; sleep 约500ms就结束等,挺慢的，串口的反应
							(share talk 0);; 清除 标志，read process有返回了
							(assert (share content))
							(assert "\n")
							(net-send connection (share content))
							
							;; here deal with data
						)
					)
				)
			
		
		(assert "close connect port1\n")
		(net-close connection)
	)		
)

(define (read_serial dv)
    (setq ret "")
    (setq nr -1)
	(setq tmp "")
	(setq fd (open dv "r"))
			
	(if-not (nil? fd)
		(begin
			(setq nr (read fd buff 150))
			(setq ret "")
			
			(while (> nr 0)
				(setq upk_lst (unpack (dup "b " nr) buff))

				(dolist (y upk_lst)
					(extend ret (format "%x " y))
				)
				(setq nr (peek fd))
				(if (> nr 0)
					(setq nr (read fd buff nr))
					(setq nr -1)
				)
			)
			(close fd)
		)
	)
	ret
)


(define (read_serial_process) ;;读串口的取程,启动时，考虑没有进程占用 /dev/ttyUSB0
	(setq rusbdev nil)
		
	(while (not (= (share talk) 10000))
		(if (file? dev)
			(begin
				
				;;从串口读取数据
				(sleep 40)
				
				(setq read_data (read_serial dev) ) ;; 这儿，如果连接正常，会永远等待数据的进来，如果突然zigbee被拨掉了，也会退出来
				;(sleep 100);;; make a delay to make sure data received
				(if (= (share talk) 1)
					(begin
						(share content read_data)
						(share talk 999)
					)
				)
				;; 可能自动处理数据，比如Logger
				;(if (> (length read_data) 0)
					(assert "data:" read_data "\n")
				;)
				
			)
			(begin ;;; dev {which could be ttyUSB0 or ttyUSB1} does not exsited
				
				(if (file? dev1)
					(begin
						(setq tmp dev1)
						(setq dev1 dev)
						(setq dev tmp)
						(set_serial dev)
						;;从串口读取数据
						(sleep 40)
						(setq read_data (read_serial dev) )
						;;(sleep 300);;; make a delay to make sure data received
						(if (= (share talk) 1)
							(begin
								(share content read_data)
								(share talk 999)
							)
						)
						;; 可能自动处理数据，比如Logger
						(assert read_data)
					)
				)	
			)
		)
	)
)


(set_serial dev)
;; fork now
(setq cpid1 (spawn 'read_p (read_serial_process)))
(setq cpid2 (spawn 'tcp_p1 (tcp_8083)))



(set 'server (net-listen 8087))
	
(unless (not (nil? server)) 
	(begin
		(print "listening 8087 failed\n")
		(abort)
		(exit)
	)
)
(print "Waiting for connection on: 8087 \n")

(setf pidarray (array children_num))
(setq parray (array children_num))

(for (x 0 (- children_num 1))
        (setq (nth x pidarray) (spawn 'ibm (tcp_8087)) )
)


(define (ctrlC-handler)
	(share talk 10000)
	(abort)
	(abort)
	(share talk 19)
	(share talk 20)
	(abort)
	(exit)

)

(signal SIGINT ctrlC-handler)


;;程序设计是没有退出的，所以函数的返回要及时 ,函数应该只考虑正确情况, openwrt BARRIER BREAKER (Bleeding Edge, r34253)
;;当newlisp退出时，所有没有close的open 的handle都会被 close掉
;;主要是openwrt下面使用的
;;多进程操作，操作的结果，可能会流入不对的人手里
;;设计不用考虑顺序的数据，把读来的数据，依照数据头，进行处理
;;处理数据，很多处据是没用的，openwrt只接收认为需要的数据

;; 策略是无序的，因为策略不是一个人，是多个人，任务永远是平行的，要说顺序也只能根据策略中的延时来决定谁先谁后了。

(constant 'PORT1 8085)
(constant 'PORT2 8086)
(constant 'ReadMax 138)
(constant 'SIGINT 2)
(constant 'DEV0 "/dev/ttyUSB0")
(constant 'DEV1 "/dev/ttyUSB1")

(define (get-parent_id) (sys-info 6))
(define (getpid) (sys-info 7))

(define (assert)
	
	(setq ret  "") 
	;(doargs (i) (extend ret (string i))) 
	
	(dolist (x (args))
		(extend ret (string x))
	)
	(write 1 ret ) ;; 强制显示在stdout上
)

(setq help_string (string "AT&PAN=xx xx, yy yy | Set xx xx's PanID to yy yy\n"
						"AT&SW=xx xx,pn yy yy yy,pn zz zz zz | Set xx xx's Switch's command content, pn is switch number ,will only be [0a ,0c 08,00] \n"
						"AT&PWM=xx xx,rr gg bb aa | Set xx xx's PWM four channels values rr-aa only will be from 00 to FF\n"
						"AT&PWM=rr,gg,bb,aa | set local device's PWM four channels values rr-aa only will be from 00 to FF \n"
					)
)

(setq children_num 2);; how many processes forked

(set 'talk (share))     (share talk 0)
(set 'content (share))  (share content "")
(setq going (share))  (share going nil) ;; 策略用一个share going来标识此时，是否有人工的数据进行输入，要避开和人工命令一起输入

(set 'sid (semaphore))
(semaphore sid 1);;默认不是0 

(global 'cmd_line)
(setq cmd_line nil)

(if (or (= ostype "Win32") (= ostype "Cygwin")) ;;设定设备名称，至于S10，也未必一定是S10,具体视cygwin环境
	(setq dev "/dev/ttyS10") ;; on win32 with cygwin!
	(setq dev DEV0) ;; all linux platform,espcially in openwrt 
)

(setq dev1 DEV1);; 这儿没有考虑win了

;;; load策略文件
(if (file? "base.lsp")
	(load "base.lsp")
	(new Tree 'BASE);基本策略，永远在执行得到基本环境信息的功能
)

(define (hextostring hex_buf); xxxxxxx to xx xx xx xx
	(setq nr (length hex_buf))
	(setq ret "")
	(setq upk_lst (unpack (dup "b " nr) hex_buf))

	(dolist (y upk_lst)
		(extend ret (format "%02x " y))
	)
	
	(chop ret 1)
)

(define (strtohex str);;xx xx xx to hex
	(setq bn (length (parse str " ")))
	(setq bstr (chop (dup "b" bn) 1) )
	(setq ret nil)
	
	(dolist (x (parse str " "))
		(extend ret (pack "b" (int (char x) 0 16)) );; char 函数只能处理 xx, 多了x不处理
	)
	ret
)

(define (make_serial_cmd cmd str)
	(setq cmd_line nil)
	
	(if (and (not (nil? cmd)) (not (nil? str)) ) ;; 有命令，并且带了参数
		(begin
			(cond	
				((= (trim cmd ) "AT+PING");; AT+PING=XX XX,虽然ping一个已知了短地址的设备看上去很没意义，这就是为了检查这个设备是否活着，没有坏。
					(if (list? str);;
						(begin
							(setq t1 (parse (str 0) " "))
							(if (= (length t1) 2) ;; 严格限制输入的格式，必须为 xx xx,只有一个空格，不能有多余的空格
								(setq cmd_line (string (str 0) " 70 69 6e 67"))
							)
						)
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
		(and (not (nil? cmd)) (nil? str)) ;; 表示只有命令，没有参数
		(begin
			(assert "only cmd \n")
			(cond 
				((= (trim cmd ) "AT+PING") ;; 没有参数，就表示是发给串口的，不是发向网络中某设备
						(setq cmd_line (string "70 69 6e 67"))
				)
				((= (trim cmd) "AT&ROUTER")
					(setq cmd_line (string "e0"))
				)
				((= cmd "AT&COORDI")
					(assert "set to coordi\n")
					(setq cmd_line (string "c0"))
					(assert cmd_line)
				)
			)
		)
	)
)

(define (parse_atcmd str)
	(setq ret nil)
	
	(cond 
		((starts-with str "AT+")
			(begin
				(setq tmplst (parse str "="))
				(setq cmd (tmplst 0) )
				(setq content nil)
				(if (> (length  tmplst) 1)
					(begin
						(setq content (tmplst 1))
					)
				)
				(setq ret (make_serial_cmd cmd content) )
			)
		)
		( (starts-with str "AT&")
			(begin
				(setq tmplst (parse str "="))
				(setq cmd (tmplst 0))
				
				(setq content_lst nil)
				(if (> (length tmplst)  1)
					(begin
						(setq content_lst (parse ( tmplst 1 ) ","))
					)
				)
				(make_serial_cmd (nth 0 tmplst) content_lst)
				(setq ret (copy cmd_line))
			)
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

;;;憨豆呆他
(define (hanle_serial_data data);; 处理来自串口的数据，基本上就是得到数据，存入数据库，就这样，很重要的核心功能
;; 要处理的data,可能是一只数据，也可能是几只数据粘在一条数据链上发过来的，例如，温度或是温度+湿度
;; 每个数据除了唯一性的数据头之外，还必须带上发出设备的短地址，以区别是哪个房间的数据
;; 数据结构永远是 头+短地址+内容
;; 在数据库中，每个数据是以：短地址_数据名称 这样的key存在 
;; 所有短地址（设备）的素引则是名称为 ALL_DEV 的key

	(setq len 0)
	(setq pos 0)
	(setq len (length data))
	
	
	(while (< pos (- len 1))
		(if (and (= (char (nth pos data))  0xa0) (<= (+ pos 4) len)) ;; 温度,并且长度超过当前位置 2 个单位
			(begin
				(setq tmp nil)
				(setq short_address nil)
				(setq short_address (| (<< (char (nth (+ pos 1) data) ) 8 ) (char (nth (+ pos 2) data) ) ) )
				(setq tmp (| (<< (char (nth (+ pos 3) data) ) 8 ) (char (nth (+ pos 4) data) ) ) )
				
				(BASE (string (format "%04X" short_address) "_Temp") tmp)
				(save "base.lsp" 'BASE)
				(setq pos (+ pos 4))
			)
			(and (= (char (nth pos data))  0x01) (<= (+ pos 2) len)) ;;;取子节点短地址
			(begin
				(setq short_address nil)
				(setq short_address (string (nth (+ pos 1) data) " " (nth (+ pos 2) data))) ;; 格式就是字符串的 xx xx,这样方面查看
				(if (nil? (BASE "NODES"))
					(begin
						(BASE "NODES" (list short_address))
					)
					(not (nil? (BASE "NODES")))
					(begin
						(if (not (find short_address (BASE "NODES")))
							(push short_address (BASE "NODES"))
						)
					)
				)
				(setq pos (+ pos 2))
			)
			(++ pos);; 默认是一个一个加，一个个位的前进
		)
	)

)

(define (tcp_8087) ;; tcp 进程,专门处理串口数据; 在这儿spawn了几个进程，共同处理任务
	
	(while (!= (share talk) 19)
		(set 'connection (net-accept server))
		(net-send connection ">" 1)
		(while (not (net-select connection "r" 1000)))
		
				(while (net-select connection "w" 1000)
					(if (setq rvlen (net-receive connection buff ReadMax "\n"))
						(begin
							(share talk 1)
							(sleep 200)
							;(assert buff)
							(setq tmp (chop buff 2))
							(assert tmp " " (length tmp))
							(setq atret (parse_atcmd tmp))
							(setq atret cmd_line)
							(assert "atret: " atret "\n")
							(if-not (nil? atret)
								(begin
									(cond 
										((list? atret)
											(dolist (x atret)
											(semaphore sid 2)
											(write_string_to_serial x)
											(sleep 100);;; sleep 100ms to serial  
											)
											(semaphore sid 1)
										)
										((string? atret)
											(semaphore sid 2)
											(write_string_to_serial atret)
											(semaphore sid 1)
										)
									)
							
									(if (starts-with buff "AT+")
										(begin
										(dotimes (z 50 (= (share talk) 999)) (assert "sleep10\n") (sleep 30) );; sleep 约500ms就结束等,挺慢的，串口的反应
										(share talk 0);; 清除 标志，read process有返回了	
										(assert (share content) "\n")
									
										(net-send connection (share content))
										(share content "")
										)
									(starts-with buff "AT&")
										(begin
											(net-send connection "+OK" 3)
										)
								
									)
							
									(if (starts-with buff "help")
										(net-send connection help_string (length help_string))
									)
									(if (starts-with buff "close")
										(net-close connection)
										(net-send connection ">" 1)
									)
								)
								(nil? atret)
								(begin
									(assert "AT error\n")
								)
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
										(semaphore sid 2)
										(write_string_to_serial x)
										
										(sleep 100);;; sleep 100ms to serial  
									)
									(semaphore sid 1)
								)
								((string? atret)
										(semaphore sid 2)
										(write_string_to_serial atret)
										(semaphore sid 1)
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
					(if (setq rvlen (net-receive connection buff ReadMax "\n"))
						(begin
							(share talk 1)
							(sleep 200)
							;(assert buff)
							
							(semaphore sid 2)
							(write_string_to_serial (chop buff 1))
							(semaphore sid 1)
							
							(dotimes (z 50 (= (share talk) 999)) (assert "sleep10\n") (sleep 20) );; sleep 约500ms就结束等,挺慢的，串口的反应
							(share talk 0);; 清除 标志，read process有返回了
							(assert (share content))
							(assert "\n")
							(net-send connection (share content))
							(share content "") ;; 清空，如果发送过，就清空
							
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
				(extend ret buff)
				
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
					(assert "data:" (hextostring read_data )"\n")
					(hanle_serial_data  read_data)
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
						(assert (hextostring read_data))
						(hanle_serial_data  read_data)
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

(define (get_base_info) ;; 基本策略，一直不停的获得目前环境的数据，存入数据库
	(setq sleep_time 0)
	(while 1
		(if (= (semaphore sid) 1)
			(begin
				(write_string_to_serial "d8 ff ff a0") ;; 温湿度 ，目前是广播，以后是用遍历所有短地址的精准方式进行查询
				(sleep 1000)
			)
			(= (semaphore sid) 1)
			(begin
				(write_string_to_serial "a0") 
				(sleep 1000)
			)
		)
		
		(sleep (* 10 1000));; 间隔
		(++ sleep_time)
		(if (> sleep_time 2);; 间隔次数
			(begin
				(write_string_to_serial "d8 ff ff 01");; 得到全部的子节点
				(setq sleep_time 0)
				(sleep 1000)
			)
			
		)
		(sleep (* 10 1000));; 间隔
	)
)

(signal SIGINT ctrlC-handler)

(setq cpid3 (spawn 'base_p (get_base_info)))

(setq rules_dir "rules")

(define (load_rules)
	(if (directory? rules_dir) ;; mean rules directory exsited
		(begin
			(setq lsp_lst (directory rules_dir "\\.rul") );; 后辍来个rules,其实就是标准的 newlisp 文件
			(dolist (x lsp_lst)
				(load (string rules_dir "/" x)) ;; 必须正确，否则会出错，进程退出了,每个策略就是一个进程 
			)
		)
		;;不存在rules就不load进去
	)
)

(sleep (* 1000 5))
(load_rules)



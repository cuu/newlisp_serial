;;程序设计是没有退出的，所以函数的返回要及时 ,函数应该只考虑正确情况, openwrt BARRIER BREAKER (Bleeding Edge, r34253)
;;当newlisp退出时，所有没有close的open 的handle都会被 close掉
;;主要是openwrt下面使用的
;;多进程操作，操作的结果，可能会流入不对的人手里
;;设计不用考虑顺序的数据，把读来的数据，依照数据头，进行处理
;;处理数据，很多处据是没用的，openwrt只接收认为需要的数据

;; 策略是无序的，因为策略不是一个人，是多个人，任务永远是平行的，要说顺序也只能根据策略中的延时来决定谁先谁后了。
;; 节点数据格式是 一个list ("短地址" "Mac地址" "别的栏“）

(constant 'PORT1 8085)
(constant 'PORT2 8086)
(constant 'ReadMax 138)
(constant 'SIGINT 2)
(constant 'DEV0 "/dev/ttyUSB0");; 测试时用usb
(constant 'DEV1 "/dev/ttyATH0");; 正式版用串口

(define (get-parent_id) (sys-info 6))
(define (getpid) (sys-info 7))

(change-dir "/usr/share")
(define (assert)
	
	(setq ret  "") 
	;(doargs (i) (extend ret (string i))) 
	
	(dolist (x (args))
		(extend ret (string x))
	)
	(write 1 ret ) ;; 强制显示在stdout上
)

(define (assertln)
	(assert (args))
	(assert "\n")
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
(setq base_in_mem (share)) ;; 存放 所有数据，用共享内存的方式，减少磁盘操作

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

(define (do_plot arg_file)	
	;(setq PLUGIN_NAME "voc")
	;; store whatever the plugin defined ,the name"test_plugin_data should be unique, because this is like a namespace, newlisp has no sub namspace except under MAIN
	;(setq my_sec_name "argscmd")
	;(setq arg_file (string "rules/" PLUGIN_NAME ".arg"))
	
	(set 'in-file (open arg_file "read"))
	(if (not (nil? in-file))
		(setq abc (read-line in-file))
	)
	(while (not (nil? abc))
					(if (and  (starts-with (current-line) "[") (ends-with (current-line) "]"));; means [xxx] style 
						(begin
							(setq section_name (slice (chop (current-line) 1 ) 1 ) )
						;	(MAIN:assert "section name " section_name "\n")
							(if (= section_name my_sec_name)
								(begin
									(setq data (read-line in-file))
									(if (nil? data) (setq data ""))
									
									(while (and (not (nil? data)) (not (starts-with  data "[" ))  )
										(parse_cmd data)
										
										(setq data (read-line in-file))
									)
								;	(MAIN:assert "do my_sec_name over")
								)
							)
						)
						(setq abc (read-line in-file))
					)					
		;(sleep (* 1000 10))		
	)
);;ends

(define (parse_cmd cmd_str)
	(if (starts-with cmd_str "CMD")
		(begin
			(setq tmp (parse cmd_str "#"))
			(if (> (length tmp) 1)
				(begin
					(setq cmdstr (nth 1 tmp))
					(setq dblist (plugin_data))
				;	(MAIN:assert dblist "\n")
					(dolist (x dblist)
						(if (find (nth 0 x) cmdstr) 
							(begin
									(replace (nth 0 x) cmdstr (string (nth 1 x)));; 在之前已经处理好 DEV=XXXX 的真值了
							)
						)
					)
					;(MAIN:assert cmdstr)
					;; 其实在执行之前，必须要得到执行的许可
					(if (lambda? MAIN:write_string_to_serial)
					(begin 
						(assert cmdstr "\n")
						;(eval-string cmdstr)
					))
					
					(sleep 300);every time executed a serial command ,sleep 1s 
				)
			)
		)
		(find "=" cmd_str)
		(begin
			(setq tmp (parse cmd_str "="))
			(if (> (length tmp) 1)
				(begin
					
					(if (starts-with (nth 0 tmp) "DEV");; only handle DEV* 
							(begin
								(if (not (nil? (MAIN:BASE (string (nth 1 tmp) ))))
									(plugin_data (nth 0 tmp)  (MAIN:BASE (string (nth 1 tmp))) );; store to the Tree 
									(plugin_data (nth 0 tmp)  0 )
								)
							)
							(plugin_data (nth 0 tmp)  (nth 1 tmp))
					) 
					
				)
			)
		)
	)
)

(define (stringtohex str)
        (setq len (length str))
        (setq pos 0)
        (setq ret nil)
        (while (< pos len)
                (setq tmp (slice str pos 2))
                (extend ret (string (int tmp 0 16)))
                (extend ret " ")
                (++ pos) (++ pos)
        )
        (setq dup_str (dup "b " (/ len 2)))
        (setq pack_str (string "(pack \"" dup_str "\" " ret ")"))
        (setq ret (eval-string pack_str))
        ret
)

(define (hextostring hex_buf); xxxxxxx to xx xx xx xx
	(setq nr (length hex_buf))
	;(assert "hextostring length: " nr "\n")
	(setq ret nil)
	(setq dupstr (dup "b " nr))
	
	(if (> (length dupstr) 1)
		(begin
			(setq dupstr (chop dupstr 1))
	
		(setq upk_lst (unpack dupstr hex_buf))

		(dolist (y upk_lst)
			(extend ret (format "%02x " y))
		)
	
		(setq ret (chop ret 1))
		)
	)
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
		(setq serial_port_setting (string "stty -F " dev_str " cs8 " baud " -ixon -icanon  min 2 time 0") )
		;(setq serial_port_setting (string "stty -F " dev_str " cs8 " baud " -ignbrk -brkint -igncr －ignpar -imaxbel -opost -onlcr -isig -icanon -iexten -echo -echoe -echok -echoctl -echoke noflsh   min 150 time 1"))
		(setq serial_port_setting (string "stty -F " dev_str " cs8 " baud " raw min 1 time 0")) ;;最少2个字节，一个肯定是长度字节，另一个肯定是数所头字节，还有数据内容字节，可能是0，时间无限一直等待
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
    (write_serial ret (length ret))
)

;;;憨豆呆他
(define (handle_serial_data data);; 处理来自串口的数据，基本上就是得到数据，存入数据库，就这样，很重要的核心功能
;; 要处理的data,可能是一只数据，也可能是几只数据粘在一条数据链上发过来的，例如，温度或是温度+湿度
;; 每个数据除了唯一性的数据头之外，还必须带上发出设备的短地址，以区别是哪个房间的数据
;; 数据结构永远是 头+短地址+内容
;; 在数据库中，每个数据是以：短地址_数据名称 这样的key存在 
;; 所有短地址（设备）的素引则是名称为 ALL_DEV 的key

	(setq len 0)
	(setq pos 0)
	(setq len (length data))
	
	
	(while (< pos len)
		
			(if 
				(and (< pos len) (= (char (nth pos data))  0x06) (< (+ pos 2) len)) ;; panid，从和openwrt连着的zigbee获得
				(begin
					(setq panid (slice (format "%04X" (get-int (reverse (slice data (+ pos 1) 2)))) 0 4))
					(if (not (nil? (BASE "PANID")))
							(if (= (BASE "PANID") "FFFE")
								(BASE "PANID" panid)
								(!= (BASE "PANID" "FFFE"))
								(BASE "PANID2"  panid)
							)
							(nil? (BASE "PANID"))
								(BASE "PANID" panid)
					)
					(share base_in_mem (BASE))
					(setq pos (+ pos 3))
				)
				(and (< pos len) (= (char (nth pos data))  0xa0) (< (+ pos 4) len)) ;; 温度,并且长度超过当前位置 2 个单位
				(begin
					(setq tmp nil)
					(setq short_address nil)
					(setq short_address (slice (format "%04X" (get-int (reverse (slice data (+ pos 1) 2)))) 0 4))
					
					;(setq tmp (| (<< (char (nth (+ pos 3) data) ) 8 ) (char (nth (+ pos 4) data) ) ) )
					(setq tmp (get-int (slice data 2 2)))
					(BASE (string  short_address "_TEMP") (int tmp))
					(save "base.lsp" 'BASE)
					(share base_in_mem (BASE))
					(setq pos (+ pos 5))
				)
				(and (< pos len) (= (char (nth pos data))  0xa1) (< (+ pos 4) len)) ;; 温度,并且长度超过当前位置 2 个单位
				(begin
					(setq tmp nil)
					(setq short_address nil)	
					(setq short_address (format "%04X" (get-int (reverse (slice data (+ pos 1) 2)))))
					(setq tmp (get-int (reverse (slice data 2 2))))
					(BASE (string  (slice short_address 0 4) "_HUMI") (int tmp));; humidity 
					(share base_in_mem (BASE))
					(setq pos (+ pos 5))
				)
					
			(and (< pos len) (= (char (nth pos data))  0x02) (< (+ pos 10) len)) ;;;取子节点短地址0x02 xx xx mm mm mmm mmmmmmmmmm.,0x02+短地址+Mac地址
			(begin
				(setq short_address nil)
				(setq short_address (format "%04X" (get-int (reverse (slice data (+ pos 1) 2)))))
				(setq mac_address (hextostring (slice data (+ pos 3) 8))) 
				(setq sa_string (slice short_address 0 4))
				(setq ma_string (slice mac_address 0 8))
				
				(if (nil? (BASE "NODES"))
					(begin
						(BASE "NODES" (list (list ma_string (list "shortaddress" sa_string))))
						(share base_in_mem (BASE))
					)
					(not (nil? (BASE "NODES")))
					(begin
						(setq was_find -1)
						(setq match_ele nil)
						(dolist (w (BASE "NODES") (if (find ma_string w) (begin (setq match_ele w) (setq  was_find $idx))) ) )
							
						(if (= was_find -1)
							(begin
								(push (list ma_string  (list "shortaddress" sa_string)) (BASE "NODES"))
								(while (!= (length (share base_in_mem)) (length (BASE)) )
										(share base_in_mem (BASE))
								)
							)
							(> was_find -1);; 这个设备存在过了
							(begin
								(if (not (nil? match_ele))
									(begin
										(dolist (w match_ele) (if (find "shortaddress") (setq (nth $idx (nth was_find (BASE "NODES"))) (list "shortaddress" sa_string))))
										(while (!= (length (share base_in_mem)) (length (BASE)) )
											(share base_in_mem (BASE))
										)
									)
								)
							)
						)
						(setq was_find -1)
						(setq match_ele nil)
					)
				)
				(setq pos (+ pos 11))
				(setq sa_string nil) (setq ma_string nil)
			)
			
			(and (< pos len) (= (char (nth pos data))  0xa6) (< (+ pos 6) len)) ;;; 取pm25 a8 xx xx cc cc dd dd, c d is low and counter
			(begin
				(setq short_address nil)
				(setq short_address (slice (format "%04X" (get-int (reverse (slice data (+ pos 1) 2)))) 0 4))
				(setq tmp1 (get-int (reverse (slice data 3 2))))
				(setq tmp2 (get-int (reverse (slice data 5 2))))
				(setq pm25_ratio (div tmp1 tmp2))
				(if (<= pm25_ratio 0.008)
						(setq tmp 0)
						(and (> pm25_ratio 0.008) (<= pm25_ratio 0.1 ))
						(setq tmp 1)
						(and (> pm25_ratio 0.01 ) (<= pm25_ratio 0.14))
						(setq tmp 2)
						(and (> pm25_ratio 0.014) (<= pm25_ratio 0.40)) ;; pm25 太大了也是计算错误
						(setq tmp 3)
				) 
				(BASE (string short_address "_PM25") (int tmp))
				(share base_in_mem (BASE))
				(setq pos (+ pos 7))
			)
			(and (< pos len) (= (char (nth pos data))  0xa8) (< (+ pos 3) len)) ;; 取 voc 有害气体数值， 数值范围只有 是 00 01 10 11 四种可能a8 xx xx ??
			(begin
				(setq short_address nil)
				(setq short_address (format "%04X" (get-int (reverse (slice data (+ pos 1) 2)))))
				(setq tmp (get-int (nth (+ pos 3) data))	)
				(if (nil? tmp) (setq tmp -1)
				)
				(BASE (string (slice short_address 0 4) "_VOC") (int tmp))
				(share base_in_mem (BASE))
				(setq pos (+ pos 4))			
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
										(dotimes (z 50 (= (share talk) 999)) (sleep 30) );; sleep 约500ms就结束等,挺慢的，串口的反应
										(assert z " sleep over\n")
										
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
										
										(write_string_to_serial x)
										
										(sleep 100);;; sleep 100ms to serial  
									)
									
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
		(net-send connection ">" 1)
		(while (not (net-select connection "r" 1000)))
		
				(while (net-select connection "w" 1000)
					(if (setq rvlen (net-receive connection buff ReadMax "\n"))
						(begin
							(share talk 1)
							(sleep 200)
							;(assert buff)
							(semaphore sid 2)
							(write_string_to_serial (chop buff 1))
							(semaphore sid -1)
							
							(setq abctimer 0)
							(dotimes (z 50 (= (share talk) 999)) (setq abctimer z) (sleep 20) );; sleep 约500ms就结束等,挺慢的，串口的反应
							
							(assert abctimer " times sleep over\n")
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
		(while (> (semaphore sid) 1)
				(semaphore sid -1)
		)
		(assert "semaphore sid " (semaphore sid) "\n")
	)		
)


(define (read_serial fd)
    (setq ret nil)
    (setq nr nil)
		(setq tmp nil)

			(setq nr (read fd buff 64))
			
			(extend ret buff)
			
			(if (> nr 0);; means readed sth
			  (begin
			  
			    (cond 
				((= (char (nth 0 ret)) 0x51);确保首个数据是要处理的数据，因为只有要处理的数据，才能有余下更多的数据存在
				 (begin
				    (if (< nr 2);;最少要有两个数据，如果意外只读了一个，再读一次，一个
					(begin
					  (setq nr (read fd buff 1))
					  (extend ret buff)
					)
				    )
				    (setq dlen (get-int (nth 1 ret)))
				    (while (and (< (length ret) dlen) (> dlen 1))
				      (setq nr (read fd buff (- dlen (length ret)) ));;精确读，不多读下一条数据
				      (if (not (nil? nr)) (extend ret buff) )
				    )
				 )
				)
				
				
				
			    )
			  )
			)
			 
			;(while (not (nil? nr))
			;	(extend ret buff)
				
			;	(setq nr (peek fd))
			;  (sleep 100)
			;	(setq nr (peek fd))
				;(if (> nr 0)
			;		(setq nr (read fd buff (+ nr 10) ))
				;)
			;)
	ret
)


(define (read_serial_process) ;;读串口的取程,启动时，考虑没有进程占用 /dev/ttyUSB0,顺便处理策略
		
	(while (not (= (share talk) 10000))
		(if (file? dev)
			(begin
				;;从串口读取数据
				(if (nil? rusbdev);; 如果usbdev 重新插上了
					(setq rusbdev (open dev "r"))
				)
				(setq read_data (read_serial rusbdev) ) ;; 这儿，如果连接正常，会永远等待数据的进来，如果突然zigbee被拨掉了，也会退出来
				(assert "read_data length: " (length read_data) "\n")
				;(sleep 100);;; make a delay to make sure data received
				(if (= (share talk) 1)
					(begin
						(share content read_data)
						(while (!= (length (share content)) (length read_data))
							(share content read_data)
						)
						(share talk 999)
					)
				)
				;; 可能自动处理数据，比如Logger
				;(if (> (length read_data) 0)
					(assert "data:" (hextostring read_data ) " | " read_data "\n")
					(handle_serial_data  read_data)
				;)
				
			)
			(if-not (file? dev);;如果突然 usb设备被拨掉，这儿就要清除掉这个rusbdev
				(begin
					(if-not (nil? rusbdev) (close rusbdev))
					(setq rusbdev nil)
				)
			)
		)
	)
)

(while (not  (file? dev))
	(sleep 5000)
	(println "waiting for device to be plugged")
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

(define (base_plot) ;; 基本策略，一直不停的获得目前环境的数据，存入数据库
	(setq sleep_time 0)

	(new Tree 'plugin_data)
	
	(while 1
		
		(if (and (not (nil? (share base_in_mem))) (list? (share base_in_mem)))
			(begin
				(dolist (u (share base_in_mem) 
				(if (find "NODES" u)
						(setq nlst (nth (- (length u) 1) u))
						(find "PANID" u)
						(setq panid (nth (- (length u) 1) u))
						(find "PANID2" u)
						(setq panid2 (nth (- (length u) 1) u))
				)
				)
				)
			)
		)
		(silent(if 
			(and (nil? panid) (nil? panid2))
			(begin
				(write_string_to_serial "05")
			)
			(and (not (nil? panid)) (nil? panid2))
			(begin
				(assert panid "\n")
				(if (= panid "FFFE")
					;;to be a coordinator
					(begin
						(write_string_to_serial "c0")
					)
				)
			)
			(and (not (nil? panid)) (not (nil? panid2)))
			(begin
				(if
					(and (not (= panid "FFFE")) (not (= panid2 "FFFE")))
					(begin
						(if (!= panid panid2)
							(begin		
								(write_string_to_serial (string "70 " (slice panid 0 2) " " (slice panid 2 2)))
								
							)
						)
					)
				)
			)
		))
		(if (and (not (nil? nlst)) (> (length nlst) 0))
			(begin
				(dolist (d nlst (if (and (find d "shortaddress") (> (length d) 1)) (setq dst_addr (nth 1 d))))
				)
				(if (not (nil? dst_addr))
				)				
			)
		)
		(sleep 1000)
		;;;策略
	(setq rules_dir "rules")
	(if (directory? rules_dir) ;; mean rules directory exsited
		(begin
			(setq lsp_lst (directory rules_dir "\\.arg") );; 后辍来个rules,其实就是标准的 newlisp 文件
			(dolist (x lsp_lst)
				(do_plot (string rules_dir "/" x)) ;; 必须正确，否则会出错，进程退出了,每个策略就是一个进程 
				(sleep 400);;给cc2530必要的”休息“时间
			)
		)
		;;不存在rules就不load进去
	)		
		(write_string_to_serial "50") ;; ping 
		(sleep 300)
	)
)

(signal SIGINT ctrlC-handler)

(setq cpid3 (spawn 'base_p (base_plot)))



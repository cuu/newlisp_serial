;;���������û���˳��ģ����Ժ����ķ���Ҫ��ʱ ,����Ӧ��ֻ������ȷ���, openwrt BARRIER BREAKER (Bleeding Edge, r34253)
;;��newlisp�˳�ʱ������û��close��open ��handle���ᱻ close��
;;��Ҫ��openwrt����ʹ�õ�
;;����̲����������Ľ�������ܻ����벻�Ե�������
;;��Ʋ��ÿ���˳������ݣ��Ѷ��������ݣ���������ͷ�����д���
;;�������ݣ��ܶദ����û�õģ�openwrtֻ������Ϊ��Ҫ������

;; ����������ģ���Ϊ���Բ���һ���ˣ��Ƕ���ˣ�������Զ��ƽ�еģ�Ҫ˵˳��Ҳֻ�ܸ��ݲ����е���ʱ������˭��˭���ˡ�

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
	(write 1 ret ) ;; ǿ����ʾ��stdout��
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
(setq going (share))  (share going nil) ;; ������һ��share going����ʶ��ʱ���Ƿ����˹������ݽ������룬Ҫ�ܿ����˹�����һ������

(set 'sid (semaphore))
(semaphore sid 1);;Ĭ�ϲ���0 

(global 'cmd_line)
(setq cmd_line nil)

(if (or (= ostype "Win32") (= ostype "Cygwin")) ;;�趨�豸���ƣ�����S10��Ҳδ��һ����S10,������cygwin����
	(setq dev "/dev/ttyS10") ;; on win32 with cygwin!
	(setq dev DEV0) ;; all linux platform,espcially in openwrt 
)

(setq dev1 DEV1);; ���û�п���win��

;;; load�����ļ�
(if (file? "base.lsp")
	(load "base.lsp")
	(new Tree 'BASE);�������ԣ���Զ��ִ�еõ�����������Ϣ�Ĺ���
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
		(extend ret (pack "b" (int (char x) 0 16)) );; char ����ֻ�ܴ��� xx, ����x������
	)
	ret
)

(define (make_serial_cmd cmd str)
	(setq cmd_line nil)
	
	(if (and (not (nil? cmd)) (not (nil? str)) ) ;; ��������Ҵ��˲���
		(begin
			(cond	
				((= (trim cmd ) "AT+PING");; AT+PING=XX XX,��Ȼpingһ����֪�˶̵�ַ���豸����ȥ��û���壬�����Ϊ�˼������豸�Ƿ���ţ�û�л���
					(if (list? str);;
						(begin
							(setq t1 (parse (str 0) " "))
							(if (= (length t1) 2) ;; �ϸ���������ĸ�ʽ������Ϊ xx xx,ֻ��һ���ո񣬲����ж���Ŀո�
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
		(and (not (nil? cmd)) (nil? str)) ;; ��ʾֻ�����û�в���
		(begin
			(assert "only cmd \n")
			(cond 
				((= (trim cmd ) "AT+PING") ;; û�в������ͱ�ʾ�Ƿ������ڵģ����Ƿ���������ĳ�豸
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
	(setq baud 38400) ;;������
	(if (or (= ostype "Win32") (= ostype "Cygwin")) ;;��������ָ��
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

;;;��������
(define (hanle_serial_data data);; �������Դ��ڵ����ݣ������Ͼ��ǵõ����ݣ��������ݿ⣬������������Ҫ�ĺ��Ĺ���
;; Ҫ�����data,������һֻ���ݣ�Ҳ�����Ǽ�ֻ����ճ��һ���������Ϸ������ģ����磬�¶Ȼ����¶�+ʪ��
;; ÿ�����ݳ���Ψһ�Ե�����ͷ֮�⣬��������Ϸ����豸�Ķ̵�ַ�����������ĸ����������
;; ���ݽṹ��Զ�� ͷ+�̵�ַ+����
;; �����ݿ��У�ÿ���������ԣ��̵�ַ_�������� ������key���� 
;; ���ж̵�ַ���豸����������������Ϊ ALL_DEV ��key

	(setq len 0)
	(setq pos 0)
	(setq len (length data))
	
	
	(while (< pos (- len 1))
		(if (and (= (char (nth pos data))  0xa0) (<= (+ pos 4) len)) ;; �¶�,���ҳ��ȳ�����ǰλ�� 2 ����λ
			(begin
				(setq tmp nil)
				(setq short_address nil)
				(setq short_address (| (<< (char (nth (+ pos 1) data) ) 8 ) (char (nth (+ pos 2) data) ) ) )
				(setq tmp (| (<< (char (nth (+ pos 3) data) ) 8 ) (char (nth (+ pos 4) data) ) ) )
				
				(BASE (string (format "%04X" short_address) "_Temp") tmp)
				(save "base.lsp" 'BASE)
				(setq pos (+ pos 4))
			)
			(and (= (char (nth pos data))  0x01) (<= (+ pos 2) len)) ;;;ȡ�ӽڵ�̵�ַ
			(begin
				(setq short_address nil)
				(setq short_address (string (nth (+ pos 1) data) " " (nth (+ pos 2) data))) ;; ��ʽ�����ַ����� xx xx,��������鿴
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
			(++ pos);; Ĭ����һ��һ���ӣ�һ����λ��ǰ��
		)
	)

)

(define (tcp_8087) ;; tcp ����,ר�Ŵ���������; �����spawn�˼������̣���ͬ��������
	
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
										(dotimes (z 50 (= (share talk) 999)) (assert "sleep10\n") (sleep 30) );; sleep Լ500ms�ͽ�����,ͦ���ģ����ڵķ�Ӧ
										(share talk 0);; ��� ��־��read process�з�����	
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

(define (tcp_8084) ;; tcp ����,�����Ҫ����������openwrt�ϵ����ݿ�֮�࣬����״̬���ڵ��������;; telnet �õ�
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

(define (tcp_8083) ;; tcp ����,ר�Ŵ���������
	
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
							
							(dotimes (z 50 (= (share talk) 999)) (assert "sleep10\n") (sleep 20) );; sleep Լ500ms�ͽ�����,ͦ���ģ����ڵķ�Ӧ
							(share talk 0);; ��� ��־��read process�з�����
							(assert (share content))
							(assert "\n")
							(net-send connection (share content))
							(share content "") ;; ��գ�������͹��������
							
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


(define (read_serial_process) ;;�����ڵ�ȡ��,����ʱ������û�н���ռ�� /dev/ttyUSB0
	(setq rusbdev nil)
		
	(while (not (= (share talk) 10000))
		(if (file? dev)
			(begin
				
				;;�Ӵ��ڶ�ȡ����
				(sleep 40)
				
				(setq read_data (read_serial dev) ) ;; ����������������������Զ�ȴ����ݵĽ��������ͻȻzigbee�������ˣ�Ҳ���˳���
				;(sleep 100);;; make a delay to make sure data received
				(if (= (share talk) 1)
					(begin
						(share content read_data)
						(share talk 999)
					)
				)
				;; �����Զ��������ݣ�����Logger
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
						;;�Ӵ��ڶ�ȡ����
						(sleep 40)
						(setq read_data (read_serial dev) )
						;;(sleep 300);;; make a delay to make sure data received
						(if (= (share talk) 1)
							(begin
								(share content read_data)
								(share talk 999)
							)
						)
						;; �����Զ��������ݣ�����Logger
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

(define (get_base_info) ;; �������ԣ�һֱ��ͣ�Ļ��Ŀǰ���������ݣ��������ݿ�
	(setq sleep_time 0)
	(while 1
		(if (= (semaphore sid) 1)
			(begin
				(write_string_to_serial "d8 ff ff a0") ;; ��ʪ�� ��Ŀǰ�ǹ㲥���Ժ����ñ������ж̵�ַ�ľ�׼��ʽ���в�ѯ
				(sleep 1000)
			)
			(= (semaphore sid) 1)
			(begin
				(write_string_to_serial "a0") 
				(sleep 1000)
			)
		)
		
		(sleep (* 10 1000));; ���
		(++ sleep_time)
		(if (> sleep_time 2);; �������
			(begin
				(write_string_to_serial "d8 ff ff 01");; �õ�ȫ�����ӽڵ�
				(setq sleep_time 0)
				(sleep 1000)
			)
			
		)
		(sleep (* 10 1000));; ���
	)
)

(signal SIGINT ctrlC-handler)

(setq cpid3 (spawn 'base_p (get_base_info)))

(setq rules_dir "rules")

(define (load_rules)
	(if (directory? rules_dir) ;; mean rules directory exsited
		(begin
			(setq lsp_lst (directory rules_dir "\\.rul") );; �������rules,��ʵ���Ǳ�׼�� newlisp �ļ�
			(dolist (x lsp_lst)
				(load (string rules_dir "/" x)) ;; ������ȷ���������������˳���,ÿ�����Ծ���һ������ 
			)
		)
		;;������rules�Ͳ�load��ȥ
	)
)

(sleep (* 1000 5))
(load_rules)



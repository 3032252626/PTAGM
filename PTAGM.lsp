;;; ==========================================================
;;;  PTAGM.lsp  ——  转角点坐标批量标注（CAD 原生多重引线版）
;;;
;;;  版本：v2.1        整理日期：2026-10-17
;;;
;;;  【命令一览】短命令，在第 5 节统一登记，想改名只改那一处
;;;    ZB    转角点坐标批量标注（主命令）
;;;    ZBB   在图中插入坐标表（只含已标注的点）
;;;    ZBO   把坐标导出成 CSV 文件
;;;    ZBL   在命令行列出转角点坐标
;;;    ZBD   清空标注图层
;;;    旧名 PTAGM / PTAGMTAB / PTAGMOUT / PTAGMLST / PTAGMDEL 保留为别名
;;;
;;;  【用法】
;;;    1. APPLOAD 加载本文件；
;;;    2. ZB  -> 选红线 -> 定筛选条件 -> 自动生成三行多重引线：
;;;               点号
;;;               X=北N（取 CAD 的 Y）
;;;               Y=东E（取 CAD 的 X）
;;;    3. ZBB 插表 / ZBO 导出 / ZBL 看列表 / ZBD 清空。
;;;
;;;  【坐标口径】写死：X=北N 取 CAD 的 Y，Y=东E 取 CAD 的 X。
;;;  【适用】AutoCAD 2008+ / 中望 CAD / 浩辰 CAD（需支持多重引线）
;;;
;;;  【更新记录】
;;;    v1.0  初版：三点法多重引线 + CSV 导出。
;;;    v1.1  修正标注文字里的换行符（错用 \X，正确写法是 \P）；
;;;          引线改为两点法，消除多余的折返；文字改三行左对齐。
;;;    v2.0  圆点全局宽度置 0；新增图中坐标表 ZBB；命令缩短；
;;;          全文格式统一，补错误处理。
;;;    v2.1  修 _pt-askfilter 里两处 _pt-getreal 漏传默认值导致的"参数太少"；
;;;          出错时一并还原原来的 *error* 处理函数。
;;; ==========================================================

(vl-load-com)

;; ============ 1. 常量与全局变量 ============
;; 默认值都在这里：改图层名、引线样式、默认字高等只动这一节。
(setq *PTAGM-VER*      "v2.1"        ; 版本号
      *PTAGM-LAYER*    "坐标标注"    ; 标注图层名（颜色 3 = 绿）
      *PTAGM-STYLE*    "ZBBZ_ML"     ; 多重引线样式名
      *PTAGM-LIST*     nil           ; 已标注点的结果表
      *PTAGM-H*        3.0           ; 最近一次使用的字高（供表格复用）
      *PTAGM-DEC*      3             ; 最近一次使用的小数位
      *PTAGM-OLDECHO*  nil           ; 出错恢复用的临时量
      *PTAGM-OLDLAYER* nil
      *PTAGM-OLDERR*   nil)

;; ============ 2. 通用小工具 ============
(defun _pt-d2r (deg) (* pi (/ deg 180.0)))
(defun _pt-r2d (rad) (/ (* rad 180.0) pi))
(defun _pt-2d  (pt)  (list (car pt) (cadr pt)))

;; 角度归一到 (-pi, pi]
(defun _pt-norm (a)
  (while (> a pi) (setq a (- a (* 2.0 pi))))
  (while (< a (- pi)) (setq a (+ a (* 2.0 pi))))
  a)

(defun _pt-fmt (val dec) (rtos val 2 dec))

;; 取用户输入的首字母并转大写；输入为空返回 nil
(defun _pt-key (s)
  (if (or (null s) (= s "")) nil (strcase (substr s 1 1))))

(defun _pt-getstr  (msg def / v) (setq v (getstring msg)) (if (or (null v) (= v "")) def v))
(defun _pt-getreal (msg def / v) (setq v (getreal  msg)) (if (null v) def v))
(defun _pt-getint  (msg def / v) (setq v (getint   msg)) (if (null v) def v))
(defun _pt-getdist (msg def / v)
  (setq v (getdist msg))
  (if (or (null v) (<= v 0.0)) def v))

;; 取出 LWPOLYLINE 图元里的所有顶点
(defun _pt-pts (ed / pts x)
  (setq pts nil)
  (foreach x ed (if (= 10 (car x)) (setq pts (cons (cdr x) pts))))
  (reverse pts))

;; 偏转角（度，带正负号：正为左转，负为右转）
(defun _pt-defl (V prev next)
  (_pt-r2d (_pt-norm (- (angle V next) (angle prev V)))))

;; 外角平分线方向（引线伸向多边形外侧）
(defun _pt-outer (V prev next / a1 a2 sx sy len)
  (setq a1 (angle V prev) a2 (angle V next)
        sx (+ (cos a1) (cos a2))
        sy (+ (sin a1) (sin a2))
        len (sqrt (+ (* sx sx) (* sy sy))))
  (if (< len 1e-9) nil (+ (atan sy sx) pi)))

;; 坐标口径换算：X=北N 取 CAD 的 Y，Y=东E 取 CAD 的 X
(defun _pt-xf (p) (list (cadr p) (car p)))

;; 是否已经存在几乎重合的点
(defun _pt-dup (V used tol / r u)
  (setq r nil)
  (foreach u used (if (<= (distance V u) tol) (setq r T)))
  r)

;; 把多段线（圆点）的全局宽度清零
(defun _pt-zerow (e / ed nd x)
  (setq ed (entget e) nd nil)
  (foreach x ed
    (setq nd (cons (if (member (car x) '(40 41 43)) (cons (car x) 0.0) x) nd)))
  (entmod (reverse nd))
  (entupd e))

;; 点表 -> Variant（AddMLeader 用）
(defun _pt-ptsvar (pts / arr i p)
  (setq arr (vlax-make-safearray vlax-vbdouble (cons 0 (1- (* 3 (length pts))))))
  (setq i 0)
  (foreach p pts
    (vlax-safearray-put-element arr i       (car p))
    (vlax-safearray-put-element arr (1+ i)  (cadr p))
    (vlax-safearray-put-element arr (+ i 2) 0.0)
    (setq i (+ i 3)))
  (vlax-make-variant arr))

;; 取 AcadAcCmColor 对象（不支持时返回 nil）
(defun _pt-accmcolor (idx / ver v c)
  (setq ver (substr (getvar "ACADVER") 1 2)
        c   (vl-catch-all-apply
              (function
                (lambda ()
                  (setq v (vlax-create-object (strcat "autocad.accmcolor." ver)))
                  (vla-put-ColorIndex v idx)
                  v))))
  (if (vl-catch-all-error-p c) nil c))

;; 单个属性安全赋值：失败也不影响后续属性
(defun _pt-try (obj prop val)
  (vl-catch-all-apply (function (lambda () (vlax-put-property obj prop val)))))

;; 文字宽度估算（表格列宽用）：ASCII 0.70 字高/字，宽字符 0.55 字高/字节
(defun _pt-tw (s h / i n w)
  (setq i 1 n (strlen s) w 0.0)
  (while (<= i n)
    (setq w (+ w (if (> (ascii (substr s i 1)) 127) (* 0.55 h) (* 0.70 h)))
          i (1+ i)))
  w)

;; ============ 3. 错误处理、图层与引线样式 ============
;; 中断（ESC / 出错）时恢复被改动的系统变量
(defun _pt-err (msg)
  (if *PTAGM-OLDECHO*
    (progn (setvar "CMDECHO" *PTAGM-OLDECHO*) (setq *PTAGM-OLDECHO* nil)))
  (if *PTAGM-OLDLAYER*
    (progn (setvar "CLAYER" *PTAGM-OLDLAYER*) (setq *PTAGM-OLDLAYER* nil)))
  (if (and msg (not (member msg '("Function cancelled" "quit / exit abort"))))
    (princ (strcat "\n[PTAGM] 出错：" msg)))
  (if *PTAGM-OLDERR*
    (progn (setq *error* *PTAGM-OLDERR* *PTAGM-OLDERR* nil)))
  (princ))

;; 建立并确保图层处于"开、解冻、解锁"状态
(defun _pt-layer (name color / old)
  (setq old (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)
  (if (null (tblsearch "LAYER" name))
    (command "_.LAYER" "_N" name "_C" color name ""))
  (command "_.LAYER" "_C" color name "_ON" name "_T" name "_U" name "")
  (setvar "CMDECHO" old)
  name)

;; 多重引线样式：斜段 + 水平肩线 + 多行文字
(defun _pt-style (name h arrowSym arrowSize / doc dict mldict sty col)
  (setq doc    (vla-get-activedocument (vlax-get-acad-object))
        dict   (dictsearch (namedobjdict) "ACAD_MLEADERSTYLE")
        mldict (vlax-ename->vla-object (cdr (assoc -1 dict)))
        sty    (vl-catch-all-apply 'vla-item (list mldict name)))
  (if (vl-catch-all-error-p sty)
    (setq sty (vlax-invoke-method mldict 'AddObject name "AcDbMLeaderStyle")))
  (_pt-try sty 'ContentType                   2)          ; 多行文字内容
  (_pt-try sty 'EnableDogleg                  :vlax-true) ; 保留水平肩线
  (_pt-try sty 'EnableLanding                 :vlax-true)
  (_pt-try sty 'DoglegLength                  (* 2.0 h))
  (_pt-try sty 'LandingGap                    (* 0.2 h))
  (_pt-try sty 'ArrowSymbol                   arrowSym)
  (_pt-try sty 'ArrowSize                     arrowSize)
  (_pt-try sty 'TextHeight                    h)
  (_pt-try sty 'TextStyle                     (getvar "TEXTSTYLE"))
  (_pt-try sty 'TextAlignmentType             0)          ; 文字左对齐，三行齐头
  (_pt-try sty 'TextAttachmentDirection       0)          ; 水平附着
  (_pt-try sty 'TextLeftAttachmentType        3)          ; 3 = 文字垂直居中
  (_pt-try sty 'TextRightAttachmentType       3)
  (_pt-try sty 'TextTopAttachmentType         3)
  (_pt-try sty 'TextBottomAttachmentType      3)
  (_pt-try sty 'TextAngleType                 1)          ; 文字始终水平
  (_pt-try sty 'FirstSegmentAngleConstraint   0)
  (_pt-try sty 'SecondSegmentAngleConstraint  0)
  (_pt-try sty 'MaxLeaderSegmentsPoints       2)          ; 只允许两点，禁止自动加折
  (_pt-try sty 'EnableFrameText               :vlax-false)
  (_pt-try sty 'ScaleFactor                   1.0)
  (_pt-try sty 'LeaderLineWeight              -1)
  (setq col (_pt-accmcolor 256))
  (if col
    (progn
      (vl-catch-all-apply 'vlax-put-property (list sty 'TextColor col))
      (vl-catch-all-apply 'vlax-put-property (list sty 'LeaderLineColor col))))
  name)

;; ============ 4. 表格绘制小工具（ZBB 用，全部 entmake，不依赖表格对象）============
(defun _pt-dln (p1 p2 lay)
  (entmake (list '(0 . "LINE") (cons 8 lay)
                 (cons 10 (list (car p1) (cadr p1) 0.0))
                 (cons 11 (list (car p2) (cadr p2) 0.0)))))

(defun _pt-dtx (p s h lay)
  (entmake (list '(0 . "TEXT") (cons 8 lay)
                 (cons 10 (list (car p) (cadr p) 0.0))
                 (cons 40 h) (cons 1 s) (cons 7 (getvar "TEXTSTYLE"))
                 '(72 . 1) '(73 . 2)
                 (cons 11 (list (car p) (cadr p) 0.0)))))

;; ============ 5. 命令入口（短命令）============
;; 想改命令名只改本节；后面的实现函数不用动。
(defun c:ZB  () (_PTAGM-run))
(defun c:ZBB () (_PTAGM-tab))
(defun c:ZBO () (_PTAGM-out))
(defun c:ZBL () (_PTAGM-lst))
(defun c:ZBD () (_PTAGM-del))

;; 旧名兼容：确认用不到之后可整块删除
(defun c:PTAGM    () (_PTAGM-run))
(defun c:PTAGMTAB () (_PTAGM-tab))
(defun c:PTAGMOUT () (_PTAGM-out))
(defun c:PTAGMLST () (_PTAGM-lst))
(defun c:PTAGMDEL () (_PTAGM-del))

;; ============ 6. 转角点采集 ============
;; 询问筛选条件，返回 (方式 阈值 目标角 容差)
(defun _pt-askfilter (/ mode thr tgt tol)
  (setq mode (_pt-key (_pt-getstr "\n筛选方式 [G=偏转角>=阈值 / T=接近目标角+-容差] <G>: " "G")))
  (if (= mode "T")
    (progn
      (setq tgt (_pt-getreal "\n目标角度(度) <90>: " 90.0)
            tol (_pt-getreal "\n容差(度) <5>: " 5.0))
      (list "T" 0.0 tgt tol))
    (progn
      (setq thr (_pt-getreal "\n偏转阈值(度) <30>: " 30.0))
      (list "G" thr 0.0 0.0))))

;; 遍历多段线收集符合条件的转角点，每项为 (顶点 偏转角 前一点 后一点)
(defun _pt-collect (ss mode thr tgt tol side / i ed pts closed n j V prev next d ok out)
  (setq out nil i 0)
  (while (< i (sslength ss))
    (setq ed (entget (ssname ss i)))
    (if (= "LWPOLYLINE" (cdr (assoc 0 ed)))
      (progn
        (setq pts    (_pt-pts ed)
              n      (length pts)
              closed (= 1 (logand 1 (cdr (assoc 70 ed))))
              j      0)
        (while (< j n)
          (setq V (nth j pts))
          (if (and (or (> j 0) closed) (or (< j (1- n)) closed))
            (progn
              (setq prev (if (> j 0) (nth (1- j) pts) (last pts))
                    next (if (< j (1- n)) (nth (1+ j) pts) (nth 0 pts))
                    d    (_pt-defl V prev next)
                    ok   (if (= mode "T")
                           (<= (abs (- (abs d) tgt)) tol)
                           (>= (abs d) thr)))
              (if (and ok (= side "L") (< d 0)) (setq ok nil))
              (if (and ok (= side "R") (> d 0)) (setq ok nil))
              (if ok (setq out (cons (list V d prev next) out)))))
          (setq j (1+ j)))))
    (setq i (1+ i)))
  (reverse out))

;; ============ 7. 避让用文字占位框 ============
;; 三行文字（点号 / X / Y）以折点为中心上下对称
(defun _pt-nc (pre num X Y dec)
  (max (+ (strlen pre) (strlen (itoa num)))
       (+ 2 (strlen (_pt-fmt X dec)))
       (+ 2 (strlen (_pt-fmt Y dec)))))

(defun _pt-box (k2 sgn h nc Lh / w yt yb xs)
  (setq w  (* 0.68 h nc)
        xs (+ (car k2) (* sgn (+ Lh (* 0.2 h))))
        yt (+ (cadr k2) (* 1.8 h))
        yb (- (cadr k2) (* 1.8 h)))
  (if (> sgn 0)
    (list xs yb (+ xs w) yt)
    (list (- xs w) yb xs yt)))

(defun _pt-hit (a b gap)
  (and (< (nth 0 a) (+ (nth 2 b) gap)) (< (nth 0 b) (+ (nth 2 a) gap))
       (< (nth 1 a) (+ (nth 3 b) gap)) (< (nth 1 b) (+ (nth 3 a) gap))))

(defun _pt-anyhit (bx boxes gap / r u)
  (setq r nil)
  (foreach u boxes (if (_pt-hit bx u gap) (setq r T)))
  r)

;; ============ 8. 生成一个多重引线标注 ============
;; 两点法：只给"测量点"和"折点"，水平肩线交给 CAD 自动生成
(defun _pt-make (V k2 sgn h str lay style arrowSym drawDot / doc ms pv ml de Lh)
  (setq Lh (* 2.0 h))
  (setq doc (vla-get-activedocument (vlax-get-acad-object))
        ms  (vla-get-modelspace doc))
  (setq pv (_pt-ptsvar (list (_pt-2d V) (_pt-2d k2))))
  (setq ml (vl-catch-all-apply 'vla-AddMLeader (list ms pv 1)))
  (if (vl-catch-all-error-p ml)
    nil
    (progn
      (vl-catch-all-apply 'vla-put-StyleName               (list ml style))
      (vl-catch-all-apply 'vla-put-Layer                   (list ml lay))
      (vl-catch-all-apply 'vla-put-Color                   (list ml 256))
      ;; 3 = 文字垂直居中，肩线落在三行文字的高度正中
      (vl-catch-all-apply 'vla-put-TextLeftAttachmentType  (list ml 3))
      (vl-catch-all-apply 'vla-put-TextRightAttachmentType (list ml 3))
      ;; 文字在折点右侧用左对齐、在左侧用右对齐，文字始终从肩线末端向外生长
      (vl-catch-all-apply 'vla-put-TextAlignmentType       (list ml (if (> sgn 0.0) 0 2)))
      (vl-catch-all-apply 'vla-put-TextBackgroundFill      (list ml :vlax-false))
      (vl-catch-all-apply 'vla-put-TextString              (list ml str))
      (vl-catch-all-apply 'vla-put-TextHeight              (list ml h))
      (vl-catch-all-apply 'vla-Update                      (list ml))
      ;; 圆点：DONUT 外径取 0.64h（路径直径补偿成 0.32h），再把全局宽度清零
      (if drawDot
        (progn
          (setq *PTAGM-OLDLAYER* (getvar "CLAYER"))
          (setvar "CLAYER" lay)
          (vl-cmdf "_.DONUT" 0.0 (* 0.64 h) (_pt-2d V) "")
          (setq de (entlast))
          (if (and de (= "LWPOLYLINE" (cdr (assoc 0 (entget de))))) (_pt-zerow de))
          (setvar "CLAYER" *PTAGM-OLDLAYER*)
          (setq *PTAGM-OLDLAYER* nil)))
      (vlax-vla-object->ename ml))))

;; ============ 9. ZB 主命令 ============
(defun _PTAGM-run (/ ss fil side h dec symc lead pre start tolDup lay raw
                     used boxes X Y str nc num nOk nSkip V d dir id got
                     L Lh gap dls das dl da ang2 L2 k2 sg2 bx V0 cf
                     arrowSym arrowSize drawDot oldErr oldEcho)
  (setq oldErr  *error*
        oldEcho (getvar "CMDECHO")
        *PTAGM-OLDECHO* oldEcho
        *PTAGM-OLDERR*  oldErr)
  (setq *error* _pt-err)
  (setvar "CMDECHO" 0)
  (prompt (strcat "\n--- ZB 转角点坐标批量标注   " *PTAGM-VER* "   ---"))
  (princ  (strcat "\n[环境] " (getvar "PRODUCT") "   ACADVER=" (getvar "ACADVER")))
  (setq ss (ssget '((0 . "LWPOLYLINE"))))
  (if (null ss)
    (princ "\n未选中任何多段线，命令结束。")
    (progn
      (setq fil  (_pt-askfilter))
      (setq side (_pt-key (_pt-getstr "\n转角方向 [A=全部 / L=仅左转 / R=仅右转] <A>: " "A")))
      (princ "\n坐标口径：X=北N 取 CAD 的 Y，Y=东E 取 CAD 的 X（已写死）")
      (setq h   (_pt-getdist "\n文字高度 <视图高度的 2%>: "    (* 0.02 (getvar "VIEWSIZE"))))
      (setq dec (_pt-getint  "\n坐标小数位 <3>: "               3))
      (setq *PTAGM-H* h *PTAGM-DEC* dec)
      (setq symc (_pt-key (_pt-getstr "\n测量点符号 [N=无 / D=圆 / T=三角] <N>: " "N")))
      (cond
        ((= symc "T") (setq arrowSym "_Closed" arrowSize (* 0.16 h) drawDot nil))
        ((= symc "D") (setq arrowSym "None"    arrowSize 0.0        drawDot T))
        (T            (setq arrowSym "None"    arrowSize 0.0        drawDot nil)))
      (setq lead   (_pt-getdist "\n引线长度 <3 倍字高>: "         (* 3.0 h)))
      (setq pre    (_pt-getstr  "\n点号前缀 <J>: "                "J"))
      (setq start  (_pt-getint  "\n起始编号 <1>: "                1))
      (setq tolDup (_pt-getdist "\n重合点判定距离 <0.02 倍字高>: " (* 0.02 h)))
      (setq lay    (_pt-getstr  (strcat "\n标注图层 <" *PTAGM-LAYER* ">: ") *PTAGM-LAYER*))
      (setq *PTAGM-LAYER* lay)
      (_pt-layer lay 3)
      (_pt-style *PTAGM-STYLE* h arrowSym arrowSize)
      ;; 设为当前引线样式；个别 CAD 不支持时用默默跳过，不影响标注本身
      (vl-catch-all-apply 'setvar (list "CMLEADERSTYLE" *PTAGM-STYLE*))
      (setq raw (_pt-collect ss (nth 0 fil) (nth 1 fil) (nth 2 fil) (nth 3 fil) side))
      ;; 先用首点试算一遍让使用者确认坐标口径，避免整批白标
      (if raw
        (progn
          (setq V0 (car (car raw)))
          (princ (strcat "\n  首点试算：X(北)=" (rtos (cadr V0) 2 3)
                         "   Y(东)="          (rtos (car V0) 2 3)))
          (setq cf (_pt-key (_pt-getstr "\n  确认标注? [Y=继续 / Q=退出] <Y>: " "Y")))
          (if (= cf "Q") (setq raw nil))))
      (if (null raw)
        (princ "\n未找到符合条件的转角点，或已取消。")
        (progn
          (setq used  nil
                boxes nil
                num   start
                nOk   0
                nSkip 0
                L     lead
                Lh    (* 2.0 h)
                gap   (* 0.35 h)
                dls   (list 0.0 0.5 1.0 1.8 2.8 4.2 6.0)
                das   (list 0.0 12.0 -12.0 25.0 -25.0 40.0 -40.0 60.0 -60.0 85.0 -85.0))
          (foreach q raw
            (setq V (car q) d (cadr q))
            (if (_pt-dup V used tolDup)
              (setq nSkip (1+ nSkip))
              (progn
                (setq used (cons V used)
                      dir  (_pt-outer V (nth 2 q) (nth 3 q)))
                (if (null dir)
                  (setq nSkip (1+ nSkip))
                  (progn
                    (setq X   (car  (_pt-xf V))
                          Y   (cadr (_pt-xf V))
                          id  (strcat pre (itoa num))
                          str (strcat id "\\PX=" (_pt-fmt X dec) "\\PY=" (_pt-fmt Y dec))
                          nc  (_pt-nc pre num X Y dec)
                          got nil)
                    ;; 在 7 个长度 x 11 个角度的候选点里挑第一个不压盖的位置
                    (foreach dl dls
                      (if (null got)
                        (foreach da das
                          (if (null got)
                            (progn
                              (setq ang2 (+ dir (_pt-d2r da))
                                    L2   (* L (+ 1.0 dl))
                                    k2   (polar (_pt-2d V) ang2 L2)
                                    sg2  (if (> (cos ang2) 0.0) 1.0 -1.0)
                                    bx   (_pt-box k2 sg2 h nc Lh))
                              (if (null (_pt-anyhit bx boxes gap)) (setq got (list k2 sg2 bx))))))))
                    ;; 全被占满时退回默认位置
                    (if (null got)
                      (setq k2  (polar (_pt-2d V) dir L)
                            sg2 (if (> (cos dir) 0.0) 1.0 -1.0)
                            bx  (_pt-box k2 sg2 h nc Lh)
                            got (list k2 sg2 bx)))
                    (setq k2  (nth 0 got)
                          sg2 (nth 1 got)
                          bx  (nth 2 got))
                    (if bx (setq boxes (cons bx boxes)))
                    (if (_pt-make V k2 sg2 h str lay *PTAGM-STYLE* arrowSym drawDot)
                      (progn
                        (setq nOk (1+ nOk))
                        (setq *PTAGM-LIST*
                              (cons (list id X Y (car V) (cadr V) d) *PTAGM-LIST*)))
                      (princ (strcat "\n  [失败] " id "（本 CAD 可能不支持 AddMLeader）")))
                    (setq num (1+ num)))))))
          (setq *PTAGM-LIST* (reverse *PTAGM-LIST*))
          (princ (strcat "\n--- 完成：标注 " (itoa nOk) " 个，跳过 " (itoa nSkip) " 个 ---"))
          (princ "\n★ 点中标注后拖动【文字旁的夹点】，引线会自动跟随。")))))
  (setvar "CMDECHO" oldEcho)
  (setq *PTAGM-OLDECHO* nil
        *PTAGM-OLDERR*  nil
        *error*         oldErr)
  (princ))

;; ============ 10. ZBB 在图中插入坐标表（只含已标注的点）============
(defun _PTAGM-tab (/ lst n h dec lay p0 x0 ytop th rh
                     w1 w2 w3 w4 tw x1 x2 x3 x4 yh ybb ybot
                     i row yc nxt es)
  (if (null *PTAGM-LIST*)
    (princ "\n暂无标注结果，请先运行 ZB。")
    (progn
      (setq lst *PTAGM-LIST* n (length lst))
      (setq dec (if *PTAGM-DEC* *PTAGM-DEC* 3))
      (setq h   (_pt-getdist (strcat "\n表格字高 <" (rtos (if *PTAGM-H* *PTAGM-H* 3.0) 2 2) ">: ")
                             (if *PTAGM-H* *PTAGM-H* 3.0)))
      (setq lay (_pt-getstr (strcat "\n表格图层 <" *PTAGM-LAYER* ">: ") *PTAGM-LAYER*))
      (_pt-layer lay 3)
      (setq p0 (getpoint "\n表格左上角插入点: "))
      (if p0
        (progn
          (setq p0   (_pt-2d p0)
                x0   (car p0)
                ytop (cadr p0)
                th   (* 2.6 h)         ; 标题带高
                rh   (* 2.0 h))        ; 行高
          ;; 列宽按内容估算，左右各留 0.8 倍字高
          (setq w1 (_pt-tw "点号" h) w2 (_pt-tw "X" h)
                w3 (_pt-tw "Y" h)    w4 (_pt-tw "0000.00" h))
          (foreach row lst
            (setq w1 (max w1 (_pt-tw (car row) h))
                  w2 (max w2 (_pt-tw (_pt-fmt (nth 1 row) dec) h))
                  w3 (max w3 (_pt-tw (_pt-fmt (nth 2 row) dec) h))))
          (setq w1 (+ w1 (* 1.6 h)) w2 (+ w2 (* 1.6 h))
                w3 (+ w3 (* 1.6 h)) w4 (+ w4 (* 1.6 h)))
          (setq tw   (+ w1 w2 w3 w4)
                x1   (+ x0 w1) x2 (+ x1 w2) x3 (+ x2 w3) x4 (+ x3 w4)
                yh   (- ytop th)        ; 表头行顶
                ybb  (- yh rh)          ; 数据区顶
                ybot (- ybb (* n rh)))  ; 表底
          ;; 横线：外框上下 + 标题下 + 数据区分行
          (_pt-dln (list x0 ytop) (list x4 ytop) lay)
          (_pt-dln (list x0 yh)   (list x4 yh)   lay)
          (_pt-dln (list x0 ybb)  (list x4 ybb)  lay)
          (_pt-dln (list x0 ybot) (list x4 ybot) lay)
          (setq i 1)
          (while (< i n)
            (_pt-dln (list x0 (- ybb (* i rh))) (list x4 (- ybb (* i rh))) lay)
            (setq i (1+ i)))
          ;; 竖线：外框左右通高，内部分隔只在数据区段
          (_pt-dln (list x0 ytop) (list x0 ybot) lay)
          (_pt-dln (list x4 ytop) (list x4 ybot) lay)
          (foreach xx (list x1 x2 x3) (_pt-dln (list xx yh) (list xx ybot) lay))
          ;; 标题与表头
          (_pt-dtx (list (+ x0 (/ tw 2.0)) (- ytop (/ th 2.0))) "界址点坐标表" h lay)
          (_pt-dtx (list (+ x0 (/ w1 2.0)) (- yh (/ rh 2.0)))   "点号"       h lay)
          (_pt-dtx (list (+ x1 (/ w2 2.0)) (- yh (/ rh 2.0)))   "X"          h lay)
          (_pt-dtx (list (+ x2 (/ w3 2.0)) (- yh (/ rh 2.0)))   "Y"          h lay)
          (_pt-dtx (list (+ x3 (/ w4 2.0)) (- yh (/ rh 2.0)))   "边长"       h lay)
          ;; 数据行：边长取到下一行的直线距离，最后一行留空
          (setq i 0)
          (foreach row lst
            (setq yc (- ybb (* i rh) (/ rh 2.0))
                  es "")
            (if (< (1+ i) n)
              (setq nxt (nth (1+ i) lst)
                    es  (_pt-fmt (distance (list (nth 3 row) (nth 4 row))
                                           (list (nth 3 nxt) (nth 4 nxt))) 2)))
            (_pt-dtx (list (+ x0 (/ w1 2.0)) yc) (car row)                 h lay)
            (_pt-dtx (list (+ x1 (/ w2 2.0)) yc) (_pt-fmt (nth 1 row) dec) h lay)
            (_pt-dtx (list (+ x2 (/ w3 2.0)) yc) (_pt-fmt (nth 2 row) dec) h lay)
            (if (/= es "") (_pt-dtx (list (+ x3 (/ w4 2.0)) yc) es h lay))
            (setq i (1+ i)))
          (princ (strcat "\n已在图中绘制坐标表：" (itoa n) " 行（只含已标注的点）"))))))
  (princ))

;; ============ 11. ZBO 导出 CSV ============
(defun _PTAGM-out (/ fn f row)
  (if (null *PTAGM-LIST*)
    (princ "\n暂无标注结果，请先运行 ZB。")
    (progn
      (setq fn (getfiled "保存坐标表" "坐标表.csv" "csv" 1))
      (if fn
        (progn
          (setq f (open fn "w"))
          (write-line "PT,X北N,Y东E,CADX,CADY,ANGLE" f)
          (foreach row *PTAGM-LIST*
            (write-line (strcat (car row) ","
                                (rtos (nth 1 row) 2 6) ","
                                (rtos (nth 2 row) 2 6) ","
                                (rtos (nth 3 row) 2 6) ","
                                (rtos (nth 4 row) 2 6) ","
                                (rtos (nth 5 row) 2 3)) f))
          (close f)
          (princ (strcat "\n已导出 " (itoa (length *PTAGM-LIST*)) " 个点到 " fn))))))
  (princ))

;; ============ 12. ZBL 命令行列出坐标 ============
(defun _PTAGM-lst (/ ss fil raw n row V X Y)
  (setq ss (ssget '((0 . "LWPOLYLINE"))))
  (if (null ss)
    (princ "\n未选中任何多段线。")
    (progn
      (setq fil (_pt-askfilter)
            raw (_pt-collect ss (nth 0 fil) (nth 1 fil) (nth 2 fil) (nth 3 fil) "A")
            n   0)
      (foreach row raw
        (setq V (car row) X (car (_pt-xf V)) Y (cadr (_pt-xf V)) n (1+ n))
        (princ (strcat "\n  " (itoa n) "   转角 " (rtos (abs (cadr row)) 2 2)
                       "度   X=" (rtos X 2 3) "   Y=" (rtos Y 2 3))))
      (princ (strcat "\n合计 " (itoa n) " 个转角点（X=北N，Y=东E）"))))
  (princ))

;; ============ 13. ZBD 清空标注图层 ============
(defun _PTAGM-del (/ lay ss i n)
  (setq lay (_pt-getstr (strcat "\n要清理的图层 <" *PTAGM-LAYER* ">: ") *PTAGM-LAYER*))
  (setq *PTAGM-LAYER* lay)
  (setq ss (ssget "_X" (list (cons 8 lay) (cons 410 (getvar "CTAB")))))
  (if (null ss)
    (princ (strcat "\n图层 " lay " 上没有对象。"))
    (progn
      (setq i 0 n 0)
      (while (< i (sslength ss)) (entdel (ssname ss i)) (setq i (1+ i) n (1+ n)))
      (setq *PTAGM-LIST* nil)
      (princ (strcat "\n已删除 " (itoa n) " 个对象，并清空结果表。"))))
  (princ))

(princ (strcat "\nPTAGM " *PTAGM-VER* " 已加载。"
               "  命令：ZB 标注 / ZBB 插表 / ZBO 导出 / ZBL 列表 / ZBD 清理"))
(princ)

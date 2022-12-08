;;; -*- lexical-binding: t -*-

(define-derived-mode adl-mode fundamental-mode "adl"
  "major mode for editing adl code."
  (setq font-lock-defaults '((
        ("SUB .*(" . font-lock-function-name-face)
        ("//.*" . font-lock-comment-face)
        ("A
\\|ACCEPT
\\|ACCOUNT
\\|ACTIVITY
\\|ADDRESS
\\|ADMINISTRATOR
\\|AGENT
\\|ALERT
\\|ALERTING
\\|ALL
\\|ALLOW
\\|ALLOWED
\\|AND
\\|ANSWER
\\|ANSWERBACK
\\|ANSWERING
\\|ANY
\\|APPEND
\\|APPLICATION
\\|AS
\\|AT
\\|AUDIO
\\|AVAILABILITY
\\|B
\\|BACKWARDS
\\|BARRED
\\|BEEP
\\|BEFORE
\\|BEYOND
\\|BLOCK
\\|BREAK
\\|BUCKET
\\|BUSY
\\|BUT
\\|BY
\\|BYPASS
\\|C
\\|CALL
\\|CALLS
\\|CALLED
\\|CALLER
\\|CALLING
\\|CANCEL
\\|CAPABILITY
\\|CAUSE
\\|CHANGE
\\|CHANNEL
\\|CHECK
\\|CLASS
\\|CLEAR
\\|CLOSED
\\|CODE
\\|COLLECTION
\\|COLON
\\|COMMA
\\|COMPARE
\\|CONCATENATED
\\|CONGESTION
\\|CONNECTED
\\|CONSTANTS
\\|CONSTRAINT
\\|CONTACT
\\|CONTEXT
\\|COPY
\\|CRITICAL
\\|CROSS
\\|CUSTOM
\\|D
\\|DATA
\\|DATE
\\|DATETIME
\\|DAY
\\|DAYS
\\|DEBUG
\\|DECREMENT
\\|DEFAULT
\\|DEFAULTS
\\|DELAY
\\|DELEGATE
\\|DELEGATED
\\|DELETE
\\|DETACHED
\\|DETECTION
\\|DEVICE
\\|DIAL
\\|DIGITS
\\|DIRECT
\\|DISABLE
\\|DISABLED
\\|DISPLAY
\\|DISTINGUISHED
\\|DIVERSION
\\|DIVERT
\\|DIVERTED
\\|DIVIDED
\\|DROP
\\|DUE
\\|E164
\\|EFFECTIVE
\\|EMAIL
\\|EMERGENCY
\\|EMPTY
\\|ENABLE
\\|ENABLED
\\|ENQUIRY
\\|EQ
\\|EQUAL
\\|EVENT
\\|EXCEPTING
\\|EXCLUDE
\\|EXISTS
\\|EXIT
\\|EXPIRING
\\|EXPIRY
\\|EXTENDED
\\|EXTERNAL
\\|FAILED
\\|FAILURE
\\|FAX
\\|FAXES
\\|FILENAME
\\|FIND
\\|FIRST
\\|FOR
\\|FORWARDED
\\|FORWARDS
\\|FRIDAY
\\|FROM
\\|GET
\\|GOSUB
\\|GOTO
\\|GREATER
\\|GREETING
\\|GROUP
\\|HANGUP
\\|HAS
\\|HASH
\\|HELD
\\|HOLD
\\|HOLIDAY
\\|HOME
\\|HOURS
\\|ID
\\|IDLE
\\|IF
\\|IGNORED
\\|IMMEDIATELY
\\|IN
\\|INACTIVITY
\\|INBAND
\\|INBOUND
\\|INCLUDING
\\|INCOMPLETE
\\|INCREMENT
\\|INDICATION
\\|INFORM
\\|INFORMATION
\\|INFORMATIONAL
\\|INSERT
\\|INTERNAL
\\|INTERRUPT
\\|INTO
\\|INVALID
\\|INVOKE
\\|IS
\\|JOIN
\\|KEY
\\|LABEL
\\|LAUNCH
\\|LEAVE
\\|BRACE
\\|BRACKET
\\|SQBRACKET
\\|LENGTH
\\|LESS
\\|LEG
\\|LEVEL
\\|LIMIT
\\|LINK
\\|LINKED
\\|LOCAL
\\|LOCATION
\\|LOG
\\|LONG
\\|MAIL
\\|MAILBOX
\\|MANAGEMENT
\\|MANY
\\|MAX
\\|MAY
\\|ME
\\|MEDIA
\\|MEMBER
\\|MEMO
\\|MEMORIES
\\|MESSAGE
\\|MINUS
\\|MINUTES
\\|MISSED
\\|MOBILE
\\|MODE
\\|MODULUS
\\|MONDAY
\\|MONITOR
\\|MOOD
\\|MOVE
\\|MULTIPLIED
\\|MUST
\\|NAME
\\|NETWORK
\\|NETWORKS
\\|NEW
\\|NEXT
\\|NO
\\|NONE
\\|NORMAL
\\|NOT
\\|NOW
\\|NUMBER
\\|OF
\\|OFF
\\|OFFERED
\\|OFFICE
\\|OFFSET
\\|ON
\\|OPEN
\\|OPERATOR
\\|OPTIMISE
\\|OPTIMISING
\\|OPTIONAL
\\|ORIGINATING
\\|OTHER
\\|OUTGOING
\\|OUR
\\|OVERRIDE
\\|OWNER
\\|PAGER
\\|PARAMETER
\\|PARENT
\\|PARTY
\\|PASSWORD
\\|PEEK
\\|PICKED
\\|PICKUP
\\|PIN
\\|PLAY
\\|PLUGIN
\\|PLUS
\\|PORT
\\|PORTED
\\|POSITION
\\|POSTPONE
\\|PREFIX
\\|PREPARE
\\|PRESENTATION
\\|PRESERVE
\\|PREVIOUS
\\|PRIORITY
\\|PROGRAM
\\|PROGRAMERROR
\\|PROGRESS
\\|PROMPT
\\|PROMPTS
\\|PUBLISH
\\|QUEUE
\\|QUOTAS
\\|READ
\\|REATTEMPT
\\|REATTEMPTS
\\|RECEIVE
\\|RECORD
\\|RECORDING
\\|REDIRECTING
\\|REFERENCE
\\|AKAREJECT
\\|RESERVED
\\|RESOURCE
\\|RESOURCES
\\|RESPONSE
\\|RESTART
\\|RESTORE
\\|RESTRICT
\\|RETURN
\\|RETURNING
\\|BRACE
\\|BRACKET
\\|SQBRACKET
\\|RINGING
\\|ROUTE
\\|ROUTING
\\|SATURDAY
\\|SAVED
\\|SCREENING
\\|SELECT
\\|SELF
\\|SEMICOLON
\\|SEND
\\|SENDER
\\|SERVICE
\\|SET
\\|SETTINGS
\\|SHARED
\\|SHOULD
\\|SHOW
\\|SILENCE
\\|SKIP
\\|SOURCE
\\|SPACE
\\|SPEAK
\\|SPECIAL
\\|STAR
\\|START
\\|STARTUP
\\|STATE
\\|STATUS
\\|STOP
\\|AKASTRING
\\|SUB
\\|SUBSCRIBER
\\|SUCCESS
\\|SUNDAY
\\|SUPPORTS
\\|SYSTEM
\\|TAG
\\|TARGET
\\|TEAM
\\|TELEPHONE
\\|TELEPHONY
\\|TEMPORARY
\\|TERMINATED
\\|TERMINATION
\\|TEXT
\\|THURSDAY
\\|THIS
\\|TIME
\\|TIMEOUT
\\|TIMER
\\|TIMETABLE
\\|TO
\\|TOKEN
\\|TONE
\\|TONES
\\|TRANSFER
\\|TRANSFERRED
\\|TRANSLUCENT
\\|TRANSPARENT
\\|TRANSIT
\\|TRIES
\\|TRUSTED
\\|TUESDAY
\\|AKATYPE
\\|UNKNOWN
\\|UNDO
\\|UNPIN
\\|UNSUPPORTED
\\|USE
\\|USER
\\|USING
\\|UNHELD
\\|VALID
\\|VALUE
\\|VERIFIED
\\|VIRTUAL
\\|VIP
\\|VOICE
\\|VOICEMAIL
\\|WAIT
\\|WAITING
\\|WARNING
\\|WEDNESDAY
\\|WEEK
\\|WEEKS
\\|WHEN
\\|WITH
\\|WRAPPED
\\|IDENTIFIER
\\|CURRENT
\\|NEXT
\\|NUM
\\|REAL
\\|STRING
\\|import
\\|CONSTANTS"
         . font-lock-constant-face)
        (" on \\|goto " . font-lock-keyword-face)))))

(add-to-list 'auto-mode-alist '("\\.adl\\'" . adl-mode))

(add-to-list 'auto-mode-alist '("\\opensips.*.cfg\\'" . conf-mode))

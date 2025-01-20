(require 'doom-themes)

(defgroup doom-shane-theme nil
  "Options for the `material dark' theme."
  :group 'doom-themes)

(defcustom doom-material-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line.
Can be an integer to determine the exact padding."
  :group 'doom-shane-theme
  :type '(choice integer boolean))

(def-doom-theme doom-shane
  "A darker version of the Material Theme inspired by xrei"

  ;; name        default   256       16
  ((bg         '("#212121" nil       nil))           ; grey13 from list-colors-display
   (bg-alt     '("#3e3e3e" nil       nil))
   (base0      '("#171F24" "black"   "black"))
   (base1      '("#262626" "#262626" "brightblack")) ; gray15
   (base15     '("#2b2b2b" "#2b2b2b" "brightblack")) ; gray17
   (base2      '("#303030" "#303030" "brightblack")) ; gray19
   (base3      '("#3A3A3A" "#3A3A3A" "brightblack"))
   (base4      '("#4a4a4a" "#444444" "brightblack")) ; gray 29
   (base5      '("#585858" "#585858" "brightblack"))
   (base6      '("#626262" "#626262" "brightblack"))
   (base7      '("#767676" "#767676" "brightblack"))
   (base8      '("#A8A8A8" "#a8a8a8" "white"))
   (fg         '("#EEFFFF" "#e4e4e4" "brightwhite"))
   (fg-alt     '("#BFC7D5" "#bcbcbc" "white"))

   (grey base5)

   (red         '("#f57373" "#ff0000" "red"))
   (orange      '("#F78C6C" "#ff5f00" "brightred"))
   (green       '("#c3e88d" "#afff00" "green"))
   (teal        '("#44b9b1" "#00d7af" "brightgreen"))
   (yellow      '("#ffcb6b" "#ffd700" "brightyellow"))
   ;; (yellow      '("#fffc00" "#fffc00" "yellow"))
   (blue        '("#82aaff" "#5fafff" "brightblue"))
   (dark-blue   '("#7986E7" "#d7ffff" "blue"))
   (magenta     '("#c792ea" "#d787d7" "brightmagenta"))
   (violet      '("#bb80b3" "#d787af" "magenta"))
   (cyan        '("#89DDFF" "#5fd7ff" "brightcyan"))
   (dark-cyan   '("#80cbc4" "#00d7af" "cyan"))

   ;; Added by Shane
   (Blue             '("#0000ff" "#0000ff" "blue1"))
   (LightGoldenrod   '("#eedd82", "#eedd82", "LightGoldenrod"))
   (LightGoldenrod1  '("#ffec8b", "#ffec8b", "LightGoldenrod1"))
   (LightGoldenrod2  '("#eedc82", "#eedc82", "LightGoldenrod2"))
   (LightGoldenrod3  '("#cdbe70", "#cdbe70", "LightGoldenrod3"))
   (NavajoWhite      '("#ffdead", "#ffdead", "NavajoWhite"))
   (BlanchedAlmond   '("#ffebcd", "#ffebcd", "BlanchedAlmond"))
   (Yellow           '("#fffc00", "#fffc00", "yellow"))
   (White            '("#ffffff", "#ffffff", "white"))

   ;; face categories -- required for all themes
   (highlight      magenta)
   (vertical-bar   red)             ;; was base2; Typically styles the vertical divider between split windows or panes within Emacs, providing a visual separation.
   (selection      base4)           ;; Highlights Selected Items: Applied to text or UI elements that are currently selected, enhancing visibility and focus.
   (builtin        blue)            ;; Denotes Built-in Constructs: Styles built-in functions, keywords, or language constructs in programming modes, distinguishing them from user-defined elements.
   (comments       LightGoldenrod3)  ;; was base6; Styles Code Comments: Applied to comment sections in code, making them easily identifiable and visually distinct from executable code.
   (doc-comments   base6)           ;; Styles Documentation Comments: Specifically targets documentation-style comments (e.g., Javadoc, Docstrings), often used to provide detailed explanations.
   (constants      orange)          ;; Denotes Function Names: Applied to function or method names in code, helping to distinguish callable entities from other identifiers.
   (functions      blue)            ;; Denotes Function Names: Applied to function or method names in code, helping to distinguish callable entities from other identifiers.
   (keywords       cyan)            ;; Styles Language Keywords: Applied to language-specific keywords (e.g., if, else, for, while), enhancing readability and structure understanding.
   (methods        blue)            ;; Highlights Methods: Specifically targets method names in object-oriented programming languages, distinguishing them from standalone functions.
   (operators      cyan)            ;; Styles Operators: Applied to operators (e.g., +, -, *, /, ==), making them stand out within expressions and statements.
   (type           magenta)         ;; Denotes Data Types: Styles data type names (e.g., int, String, List), aiding in quick identification of variable and function return types.
   (strings        green)           ;; Styles String Literals: Applied to string values in code (e.g., "Hello, World!"), making them easily distinguishable from other data types.
   (variables      yellow)          ;; Highlights Variable Names: Styles variable identifiers, helping to differentiate them from functions, constants, and types.
   (numbers        orange)          ;; Styles Numeric Literals: Applied to numerical values in code (e.g., 42, 3.14), making them stand out within expressions and declarations.
   (region         base3)           ;; base3; Highlights Selected Regions: Applied to text regions selected by the user, enhancing visibility for operations like copying or cutting.
   (error          red)             ;; Indicates Errors: Styles error messages or problematic code segments, alerting the user to issues that need attention.
   (warning        yellow)          ;; Indicates Warnings: Styles warning messages or non-critical issues in code, providing caution without halting operations.
   (success        green)           ;; Indicates Success: Styles success messages or positive outcomes (e.g., successful compilation), providing visual confirmation of successful actions.
   (vc-modified    blue)
   (vc-added       green)
   (vc-deleted     red)

   ;; custom categories
   (modeline-bg     Blue)
   ;; (modeline-bg     base2)
   ;; (modeline-bg-alt (doom-darken bg 0.01))
   (modeline-bg-alt (doom-darken Blue 0.70))
   ;; (modeline-fg     base8)
   (modeline-fg     Yellow)
   (modeline-fg-alt comments)

   (-modeline-pad
    (when doom-material-padded-modeline
      (if (integerp doom-material-padded-modeline) doom-material-padded-modeline 4))))

  ;;;; Base theme face overrides
  (;;;; emacs

   ;;-----------------------------------------------------------------------------
   ;; Syntax Highlighting Faces
   ;;-----------------------------------------------------------------------------

   ;; Highlights other matches when searching (e.g., during incremental search)
   (lazy-highlight
    ;; :background (doom-darken green 0.5)  ;; Darkened green background for better visibility
    ;; :foreground green                      ;; Green text to distinguish highlighted matches
    :background White  ;; Darkened green background for better visibility
    :foreground Blue                      ;; Green text to distinguish highlighted matches
    :weight 'bold)                        ;; Bold weight to make matches stand out

   ;; Color for prompts in the minibuffer (e.g., "Find file: ")
   (minibuffer-prompt
    :foreground yellow)                    ;; Yellow text for clear prompt visibility

   ;; Styles the selected region in buffers (e.g., when selecting text)
   (region
    ;; :background (doom-darken dark-cyan 0.5) ;; Darkened dark-cyan background for the selected area
    ;; :foreground dark-cyan                  ;; Dark-cyan text to contrast with the background
    ;; :distant-foreground (doom-darken fg 0.2) ;; Subtle foreground color for distant text within the region
    :background White ;; Darkened dark-cyan background for the selected area
    :foreground Blue                  ;; Dark-cyan text to contrast with the background
    :distant-foreground White ;; Subtle foreground color for distant text within the region
    :extend t)                             ;; Extends the region highlighting beyond window boundaries

   ;;-----------------------------------------------------------------------------
   ;; Mode Line Faces
   ;;-----------------------------------------------------------------------------

   ;; Styles the active mode line (the line at the bottom of each window showing status)
   (mode-line
    :background modeline-bg                 ;; Background color for the active mode line
    :foreground modeline-fg                 ;; Foreground color for text in the active mode line
    :box (if -modeline-pad                  ;; Conditional box styling based on padding
             `(:line-width ,-modeline-pad :color ,modeline-bg)))

   ;; Styles the inactive mode line (mode lines of non-active windows)
   (mode-line-inactive
    :background modeline-bg-alt             ;; Alternative background color for inactive mode lines
    :foreground modeline-fg-alt             ;; Alternative foreground color for text in inactive mode lines
    :box (if -modeline-pad                  ;; Conditional box styling based on padding
             `(:line-width ,-modeline-pad :color ,modeline-bg-alt)))

   ;;-----------------------------------------------------------------------------
   ;; UI Element Faces
   ;;-----------------------------------------------------------------------------

   ;; Styles tooltips (small pop-up boxes showing information)
   (tooltip
    :background (doom-darken bg-alt 0.2)    ;; Slightly darkened alternative background for tooltips
    :foreground fg)                          ;; Foreground color for tooltip text

   ;; Styles the cursor in buffers
   (cursor
    :background "#fffc00")                      ;; Yellow cursor for high visibility

   ;;-----------------------------------------------------------------------------
   ;; Line Number Faces
   ;;-----------------------------------------------------------------------------

   ;; Highlights the current line for better cursor tracking
   (hl-line
    :background "grey28"                       ;; Base2 color as the background for the highlighted line
    ;; :background "#8b1a1a"                       ;; Base2 color as the background for the highlighted line
    :foreground nil)                        ;; No change to the text foreground

   ;; Styles the line number of the current line in the buffer
   (line-number-current-line
    :inherit '(hl-line default)              ;; Inherits attributes from 'hl-line' and 'default' faces
    :foreground White                         ;; Cyan text color for the current line number
    :distant-foreground nil                  ;; No distant foreground color
    :weight 'bold                          ;; Normal font weight
    :italic nil                              ;; Non-italicized text
    :underline nil                           ;; No underline
    :strike-through nil)                     ;; No strike-through

   ;;-----------------------------------------------------------------------------
   ;; Completion and Icomplete Faces
   ;;-----------------------------------------------------------------------------

   ;; Highlights the first difference between completion candidates
   (completions-first-difference
    :foreground Yellow)                      ;; Yellow text to indicate differences

   ;; Highlights the first match in Icomplete (minibuffer completion)
   (icomplete-first-match
    :foreground cyan                        ;; Green text for the first match
    :underline t                             ;; Underlined to emphasize the match
    :weight 'bold)                           ;; Bold weight for additional emphasis

   ;;;; doom-modeline
   (doom-modeline-buffer-path       :foreground cyan :weight 'bold)
   (doom-modeline-buffer-major-mode :inherit 'doom-modeline-buffer-path)

   ;;;; highlight-thing highlight-symbol
   (highlight-symbol-face :background region :distant-foreground fg-alt)

   ;;;; highlight-thing
   (highlight-thing :background region :distant-foreground fg-alt)

   ;;;; man <built-in>
   (Man-overstrike :inherit 'bold :foreground magenta)
   (Man-underline :inherit 'underline :foreground blue)

   ;;;; org <built-in>
   ((org-block &override) :background base2)
   ((org-block-background &override) :background base2)
   ((org-block-begin-line &override) :background base2)

   ;;;; css-mode <built-in> / scss-mode
   (css-proprietary-property :foreground orange)
   (css-property :foreground green)
   (css-selector :foreground blue)

   ;;;; dired-k
   (dired-k-commited :foreground base4)
   (dired-k-modified :foreground vc-modified)
   (dired-k-ignored :foreground cyan)
   (dired-k-added :foreground vc-added)

   ;;;; ivy
   (ivy-current-match :background base3)
   (ivy-minibuffer-match-face-2
    :inherit 'ivy-minibuffer-match-face-1
    :foreground dark-cyan :background base1 :weight 'semi-bold)

   ;;;; js2-mode
   (js2-error :foreground cyan)
   (js2-exernal-variable :foreground cyan)
   (js2-external-variable :foreground cyan)
   (js2-function-call :foreground cyan)
   (js2-function-name :foreground cyan)
   (js2-function-param :foreground violet) ;; original
   (js2-instance-member :foreground cyan)
   (js2-jsdoc-html-tag-delimiter :foreground cyan)
   (js2-jsdoc-html-tag-name :foreground cyan)
   (js2-jsdoc-tag :foreground magenta) ;; original
   (js2-jsdoc-type :foreground base8) ;; original
   (js2-jsdoc-value :foreground cyan) ;; original
   (js2-keywords :foreground cyan)
   (js2-magic-paren :foreground cyan)
   (js2-object-property :foreground yellow) ;; original
   (js2-object-property-access :foreground cyan) ;; original
   (js2-private-function-call :foreground cyan)
   (js2-private-member :foreground cyan)
   (js2-warning :foreground cyan)

   ;;;; lsp
   (lsp-headerline-breadcrumb-symbols-face :foreground base7)

   ;;;; rainbow-delimiters
   (rainbow-delimiters-depth-1-face :foreground magenta)
   (rainbow-delimiters-depth-2-face :foreground orange)
   (rainbow-delimiters-depth-3-face :foreground green)
   (rainbow-delimiters-depth-4-face :foreground cyan)
   (rainbow-delimiters-depth-5-face :foreground violet)
   (rainbow-delimiters-depth-6-face :foreground yellow)
   (rainbow-delimiters-depth-7-face :foreground blue)
   (rainbow-delimiters-depth-8-face :foreground teal)
   (rainbow-delimiters-depth-9-face :foreground dark-cyan)

   ;;;; rjsx-mode
   (rjsx-tag :foreground red)
   (rjsx-attr :foreground yellow :slant 'italic :weight 'medium)
   (rjsx-tag-bracket-face :foreground cyan)

   ;;;; Magit
   (magit-header-line :background (doom-lighten modeline-bg 0.2) :foreground green :weight 'bold
                      :box `(:line-width 3 :color ,(doom-lighten modeline-bg 0.2)))

   ;;;; Treemacs
   (treemacs-git-modified-face :foreground vc-modified)

   ;;;; Web Mode
   (web-mode-html-tag-face :foreground red)
   (web-mode-html-attr-equal-face :foreground cyan)

   ;;;; Org Mode
   (org-level-1 :foreground green)
   (org-level-2 :foreground yellow)
   (org-level-3 :foreground red)
   (org-level-4 :foreground cyan)
   (org-level-5 :foreground blue)
   (org-level-6 :foreground magenta)
   (org-level-7 :foreground teal)
   (org-level-8 :foreground violet)
   (org-todo :foreground orange)

   ;;;; css
   (css-property :foreground orange)
   (css-proprietary-property :foreground magenta)
   (css-selector :foreground yellow)))

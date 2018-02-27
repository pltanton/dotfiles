" TODO:
"  - snipptes plugin
"  - TODO, and other tag words support (hilighting easy tagging)
"  - deal with LLDB

"==============================================================================
"=== Vim settings
"==============================================================================

syntax on
set background=dark
set shell=/bin/sh
set encoding=utf-8
set spelllang=en_us,ru_ru
set showcmd                
set mouse=a
set incsearch
set nohlsearch
set ignorecase
set smartcase
set wrap
set autoread
set ruler
set hidden
set colorcolumn=80
set modeline
set modelines=5
set relativenumber
set termguicolors
set splitbelow
set splitright

let mapleader="\<Space>"

" Indents
set tabstop=4
set shiftwidth=4
set softtabstop=4
set expandtab

" History
set history=64
set undolevels=128
set undofile
set undodir=~/.config/nvim/undodir
set undolevels=1000
set undoreload=10000

" Windows
nmap <leader>s<left>   :leftabove  vnew<cr>
nmap <leader>s<right>  :rightbelow vnew<cr>
nmap <leader>s<up>     :leftabove  new<cr>
nmap <leader>s<down>   :rightbelow new<cr>

nnoremap <C-H> <C-W><C-H> 
nnoremap <C-J> <C-W><C-J> 
nnoremap <C-K> <C-W><C-K> 
nnoremap <C-L> <C-W><C-L> 


" Buffers
nnoremap <leader>x      :bp <bar> bd #<cr>
nnoremap <leader>n      :bn<cr>
nnoremap <leader>p      :bp<cr>

" Improved navigation on wrapped lines
nnoremap j gj
nnoremap k gk

"==============================================================================
"=== SPECIAL VIM MAGIC
"==============================================================================

" Switch to US layout on normal mode
let g:layout='dvp'
function! SetUsLayout()
    let g:layout=system('xkblayout-state print %v')
    silent ! xkblayout-state set 0
endfunction

function! RestoreLayout()
    if g:layout != 'dvp'
        silent ! xkblayout-state set 1
    endif
endfunction

autocmd InsertLeave * call SetUsLayout()
autocmd InsertEnter * call RestoreLayout()

" Save files which require root permission
cmap w!! %!sudo tee > /dev/null %

inoremap jj <Esc> " Esc is so far away without this mapping...

" Disable arrows
map <Up> <NOP>
map <Down> <NOP>
map <Left> <NOP>
map <Right> <NOP>
imap <up> <nop>
imap <down> <nop>
imap <left> <nop>
imap <right> <nop>

" Move visual blocks
vnoremap < <gv
vnoremap > >gv

" Shift-Enter
"imap  <CR><CR><Esc>-cc

" Expand visual region by pressing v
vmap v <Plug>(expand_region_expand)
vmap <C-v> <Plug>(expand_region_shrink)

"==============================================================================
"=== Plugins
"==============================================================================

call plug#begin('~/.local/share/nvim/plugged')
    "=== Editor improvements
    Plug 'godlygeek/tabular'
    Plug 'Raimondi/delimitMate'
    Plug 'neomake/neomake'
    Plug 'tpope/vim-surround'

    Plug 'Shougo/denite.nvim'
    Plug 'Shougo/unite.vim'
    Plug 'Shougo/vimfiler.vim'
    Plug 'Shougo/neomru.vim'

    Plug 'majutsushi/tagbar'
    Plug 'scrooloose/nerdcommenter'
    Plug 'tpope/vim-repeat'
    Plug 'sbdchd/neoformat'
    Plug 'itchyny/vim-cursorword'
    Plug 'mbbill/undotree'

    "=== Language extensions
    " Java
    Plug 'artur-shaik/vim-javacomplete2'
    "call dein#add('dansomething/vim-eclim')
    " Go
    Plug 'fatih/vim-go'
    " Haskell
    Plug 'eagletmt/ghcmod-vim'
    Plug 'eagletmt/neco-ghc'
    " Octave
    Plug 'jvirtanen/vim-octave'
    " Databases
    Plug 'vim-scripts/dbext.vim'
    " LaTeX
    Plug 'lervag/vimtex'
    "call dein#add('donRaphaco/neotex')
    "call dein#add('xuhdev/vim-latex-live-preview')
    " Ruby
    Plug 'vim-ruby/vim-ruby'
    Plug 'tpope/vim-rails'
    Plug 'tpope/vim-endwise'
    " Slim
    Plug 'slim-template/vim-slim'
    " CoffeScript
    Plug 'kchmck/vim-coffee-script'
    " CSS
    Plug 'cakebaker/scss-syntax.vim'
    Plug 'hail2u/vim-css3-syntax'
    " HTML
    Plug 'mattn/emmet-vim'
    " JavaScript
    Plug 'neoclide/vim-jsx-improve'
    " Json
    Plug 'elzr/vim-json'
    "" Markdown
    "function! BuildComposer(info)
      "if a:info.status != 'unchanged' || a:info.force
        "!cargo build --release
      "endif
    "endfunction
    "call dein#add('euclio/vim-markdown-composer', { 'do': function('BuildComposer') })
    " Python
    "call dein#add('davidhalter/jedi-vim')

    "=== Git
    Plug 'airblade/vim-gitgutter'

    
    "=== Visual improvements
    Plug 'squarefrog/tomorrow-night.vim'
    Plug 'chriskempson/base16-vim'
    Plug 'vim-airline/vim-airline'
    Plug 'vim-airline/vim-airline-themes'
    Plug 'lilydjwg/colorizer'

    "=== Some service fietures
    " Vim dispatch feature
    Plug 'tpope/vim-dispatch'
    Plug 'radenling/vim-dispatch-neovim'
call plug#end()



" Install dein if not exists
"let conf_dir = '~/.config/nvim/'
"let dein_path = '~/.config/nvim/repos/github.com/Shougo/dein.vim'

"set runtimepath+=~/.config/nvim/repos/github.com/Shougo/dein.vim

"if dein#load_state(conf_dir)
    "call dein#begin(conf_dir)

    "call dein#add(dein_path)
    "if !has('nvim')
        "call dein#add('roxma/nvim-yarp')
        "call dein#add('roxma/vim-hug-neovim-rpc')
    "endif

    ""=== Editor improvements
    "call dein#add('godlygeek/tabular')
    "call dein#add('Raimondi/delimitMate')
    "call dein#add('neomake/neomake')
    "call dein#add('tpope/vim-surround')

    "call dein#add('Shougo/denite.nvim')
    "call dein#add('Shougo/unite.vim')
    "call dein#add('Shougo/vimfiler.vim')
    "call dein#add('Shougo/neomru.vim')

    "call dein#add('majutsushi/tagbar')
    "call dein#add('scrooloose/nerdcommenter')
    "call dein#add('tpope/vim-repeat')
    "call dein#add('sbdchd/neoformat')
    "call dein#add('itchyny/vim-cursorword')
    "call dein#add('mbbill/undotree')

    ""=== Deoplete as completion engine
    "function! DoRemote(arg)
      "UpdateRemotePlugins
    "endfunction
    "call dein#add('Shougo/deoplete.nvim')
    "if !has('nvim')
      "call dein#add('roxma/nvim-yarp')
      "call dein#add('roxma/vim-hug-neovim-rpc')
    "endif
    "call dein#add('zchee/deoplete-jedi')
    "call dein#add('fishbullet/deoplete-ruby')
    ""call dein#add('zchee/deoplete-go', { 'do': 'make'})

    ""=== Debug
    ""call dein#add('dbgx/lldb.nvim')

    ""=== Language extensions
    "" Java
    "call dein#add('artur-shaik/vim-javacomplete2')
    ""call dein#add('dansomething/vim-eclim')
    "" Go
    "call dein#add('fatih/vim-go')
    "" Haskell
    "call dein#add('eagletmt/ghcmod-vim')
    "call dein#add('eagletmt/neco-ghc')
    "" Octave
    "call dein#add('jvirtanen/vim-octave')
    "" Databases
    "call dein#add('vim-scripts/dbext.vim')
    "" LaTeX
    "call dein#add('lervag/vimtex')
    ""call dein#add('donRaphaco/neotex')
    ""call dein#add('xuhdev/vim-latex-live-preview')
    "" Ruby
    "call dein#add('vim-ruby/vim-ruby')
    "call dein#add('tpope/vim-rails')
    "call dein#add('tpope/vim-endwise')
    "" Slim
    "call dein#add('slim-template/vim-slim')
    "" CoffeScript
    "call dein#add('kchmck/vim-coffee-script')
    "" CSS
    "call dein#add('cakebaker/scss-syntax.vim')
    "call dein#add('hail2u/vim-css3-syntax')
    "" HTML
    "call dein#add('mattn/emmet-vim')
    "" JavaScript
    "call dein#add('neoclide/vim-jsx-improve')
    "" Json
    "call dein#add('elzr/vim-json')
    """ Markdown
    ""function! BuildComposer(info)
      ""if a:info.status != 'unchanged' || a:info.force
        ""!cargo build --release
      ""endif
    ""endfunction
    ""call dein#add('euclio/vim-markdown-composer', { 'do': function('BuildComposer') })
    "" Python
    ""call dein#add('davidhalter/jedi-vim')

    ""=== Git
    "call dein#add('airblade/vim-gitgutter')

    ""=== Visual improvements
    "call dein#add('squarefrog/tomorrow-night.vim')
    "call dein#add('chriskempson/base16-vim')
    "call dein#add('vim-airline/vim-airline')
    "call dein#add('vim-airline/vim-airline-themes')
    "call dein#add('lilydjwg/colorizer')
    ""call dein#add('ryanoasis/vim-devicons')

    ""=== Some service fietures
    "" Vim dispatch feature
    "call dein#add('tpope/vim-dispatch')
    "call dein#add('radenling/vim-dispatch-neovim')

  "call dein#end()


  "call dein#save_state()
"endif

syntax on
filetype on
filetype indent on
filetype plugin on
filetype plugin indent on
syntax enable

colorscheme base16-ocean

"==============================================================================
"=== Languages configuration
"==============================================================================

"=== JavaScript
autocmd Filetype javascript setlocal ts=2 sts=2 sw=2

"=== Java
autocmd Filetype java setlocal ts=2 sts=2 sw=2

"=== Json
autocmd Filetype json setlocal ts=2 sts=2 sw=2

"=== Ruby
autocmd Filetype ruby setlocal ts=2 sts=2 sw=2
let g:rubycomplete_buffer_loading = 1
let g:rubycomplete_rails = 1

"=== Coffee
autocmd Filetype coffee setlocal ts=2 sts=2 sw=2

"=== Slim
autocmd Filetype slim setlocal ts=2 sts=2 sw=2

"=== LaTeX
autocmd Filetype tex setlocal ts=2 sts=2 sw=2

let g:vimtex_view_method='zathura'
"let g:vimtex_latexmk_progname='nvr'
let g:vimtex_view_general_viewer='zathura'
let g:vimtex_fold_manual=1
let g:vimtex_motion_matchparen=0
let g:vimtex_quickfix_open_on_warning=0
let g:vimtex_quickfix_mode=1

let g:tex_flavor = 'latex'

"=== Haskell
" Add extra source directory if editing xmonad files
autocmd BufRead,BufNewFile */xmonad/*.hs call s:add_xmonad_path()
function! s:add_xmonad_path()
    let lib_path = resolve(expand('~/.xmonad/lib'))
    " For ghcmod-vim plugin
    if !exists('b:ghcmod_ghc_options')
        let b:ghcmod_ghc_options = []
    endif
    call add(b:ghcmod_ghc_options, '-i' . lib_path)

    " For neomake
    let cur = neomake#makers#ft#haskell#ghcmod()
    let cur.args = ['-g', '-i' . lib_path, 'check']
    let b:neomake_haskell_ghcmod_maker = cur
endfunction

"=== Golang
let g:go_term_mode = "split"
let g:go_term_enabled = 1
let g:go_highlight_functions = 1
let g:go_highlight_methods = 1
let g:go_highlight_structs = 1
let g:go_highlight_interfaces = 1
let g:go_highlight_operators = 1
let g:go_highlight_build_constraints = 1


"==============================================================================
" Plugins configuration
"==============================================================================

"=== FZF
nmap <leader><tab> :FZF<CR>
set completeopt-=preview

"=== Deoplete
let g:deoplete#enable_at_startup = 1
let g:deoplete#auto_complete_delay = 100
let g:deoplete#omni#input_patterns = {}
let g:deoplete#omni#input_patterns.java = '[^. *\t]\.\w*'

" LaTeX
if !exists('g:deoplete#omni_patterns')
      let g:deoplete#omni_patterns = {}
  endif
let g:deoplete#omni_patterns.tex =
    \ '\v\\%('
    \ . '\a*cite\a*%(\s*\[[^]]*\]){0,2}\s*\{[^}]*'
    \ . '|\a*ref%(\s*\{[^}]*|range\s*\{[^,}]*%(}\{)?)'
    \ . '|hyperref\s*\[[^]]*'
    \ . '|includegraphics\*?%(\s*\[[^]]*\]){0,2}\s*\{[^}]*'
    \ . '|%(include%(only)?|input)\s*\{[^}]*'
    \ . '|\a*(gls|Gls|GLS)(pl)?\a*%(\s*\[[^]]*\]){0,2}\s*\{[^}]*'
    \ . '|includepdf%(\s*\[[^]]*\])?\s*\{[^}]*'
    \ . '|includestandalone%(\s*\[[^]]*\])?\s*\{[^}]*'
    \ . ')\m'

" Golang
let g:deoplete#sources#go#gocode_binary = $GOPATH.'/bin/gocode'
let g:deoplete#sources#go#sort_class = ['package', 'func', 'type', 'var', 'const']

" Ruby
let g:deoplete#sources#omni#input_patterns = {
\   "ruby" : '[^. *\t]\.\w*\|\h\w*::',
\}

"=== Neomake
autocmd! BufWritePost * Neomake
" Haskell
let g:neomake_haskell_enabled_makers = ['ghcmod']
" JavaScript
let g:neomake_javascript_eslint_exe = $PWD .'/node_modules/.bin/eslint'
let g:neomake_javascript_enabled_makers = ['eslint']
" Java
"let g:neomake_java_javac_delete_output = 0
"let g:neomake_java_enabled_makers = ['checkstyle']

"=== Syntastic
let g:syntastic_error_symbol = '×'
let g:syntastic_style_error_symbol = '✗'
let g:syntastic_warining_symbol = '⚠'
let g:syntastic_style_warining_symbol = '≈'

set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0
" Java
let g:syntastic_java_javac_config_file_enabled = 1
let g:syntastic_java_checkstyle_classpath = '/usr/share/checkstyle/checkstyle.jar'
let g:syntastic_java_checkstyle_conf_file = '/usr/share/checkstyle/google_checks.xml'
let g:syntastic_java_checkers = ['javac', 'checkstyle']

"=== vimfiler

let g:vimfiler_as_default_explorer = 1
call vimfiler#custom#profile('default', 'context', {
            \ 'explorer' : 1,
            \ 'winwidth' : 30,
            \ 'winminwidth' : 30,
            \ 'toggle' : 1,
            \ 'columns' : 'type',
            \ 'auto_expand': 1,
            \ 'direction' : 'topleft',
            \ 'parent': 0,
            \ 'explorer_columns' : 'type',
            \ 'status' : 1,
            \ 'safe' : 0,
            \ 'split' : 1,
            \ 'hidden': 1,
            \ 'no_quit' : 1,
            \ 'force_hide' : 0,
            \ })
nmap <silent><F10> :VimFilerExplorer<CR>

"=== Denite

nmap <leader>ff :Denite file_rec<CR>
nmap <leader>fb :Denite buffer<CR>

call denite#custom#var('file_rec', 'command',
     \ ['ag', '--follow', '--nocolor', '--nogroup', '-g', ''])
call denite#custom#option('default', 'prompt', '>')


call denite#custom#filter('matcher_ignore_globs', 'ignore_globs',
      \ [ '.git/', '.ropeproject/', '__pycache__/', 'node_modules/',
      \   'venv/', 'images/', '*.min.*', 'img/', 'fonts/', '*.png',
      \   '*.jpg', '*.png' ])

"=== Undotree

nmap <silent><F5> :UndotreeToggle<CR>

"=== Airline
set laststatus=2

let g:airline#extensions#tabline#enabled=1
let g:airline_powerline_fonts = 1
let g:Powerline_symbols='unicode'
let g:airline_theme='base16_ocean'

if !exists('g:airline_symbols')
    let g:airline_symbols = {}
endif
let g:airline_symbols.space = "\ua0"

set showcmd

"=== Tabularize
inoremap <silent> <Bar> <Bar><Esc>:call <SID>align()<CR>a

function! s:align()
  let p = '^\s*|\s.*\s|\s*$'
  if exists(':Tabularize') && getline('.') =~# '^\s*|' && (getline(line('.')-1) =~# p || getline(line('.')+1) =~# p)
    let column = strlen(substitute(getline('.')[0:col('.')],'[^|]','','g'))
    let position = strlen(matchstr(getline('.')[0:col('.')],'.*|\s*\zs.*'))
    Tabularize/|/l1
    normal! 0
    call search(repeat('[^|]*|',column).'\s\{-\}'.repeat('.',position),'ce',line('.'))
  endif
endfunction

"=== Tagbar
nmap <F8> :TagbarToggle<CR>

"=== Javacomplete2
"autocmd FileType java setlocal omnifunc=javacomplete#Complete

"=== Python
let g:deoplete#sources#jedi#python_path = system('which python')


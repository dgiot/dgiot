if &cp | set nocp | endif
let s:cpo_save=&cpo
set cpo&vim
vmap gx <Plug>NetrwBrowseXVis
nmap gx <Plug>NetrwBrowseX
vnoremap <silent> <Plug>NetrwBrowseXVis :call netrw#BrowseXVis()
nnoremap <silent> <Plug>NetrwBrowseX :call netrw#BrowseX(netrw#GX(),netrw#CheckIfRemote(netrw#GX()))
cabbr rename =getcmdpos() == 1 && getcmdtype() == ":" ? "Rename" : "rename"
let &cpo=s:cpo_save
unlet s:cpo_save
set background=dark
set backspace=2
set expandtab
set fileencodings=ucs-bom,utf-8,default,latin1
set modelines=0
set ruler
set runtimepath=~/.vim,~/.vim/bundle/Vundle.vim,~/.vim/bundle/nerdtree,~/.vim/bundle/vim-erlang-runtime,~/.vim/bundle/vim-erlang-tags,~/.vim/bundle/vim-erlang-omnicomplete,~/.vim/bundle/vim-erlang-skeletons,~/.vim/bundle/vim-trailing-whitespace,~/.vim/bundle/rename.vim,~/.vim/bundle/Vundle.vim,~/.vim/bundle/nerdtree,~/.vim/bundle/rename.vim,~/.vim/bundle/vim-erlang-omnicomplete,~/.vim/bundle/vim-erlang-runtime,~/.vim/bundle/vim-erlang-skeletons,~/.vim/bundle/vim-erlang-tags,~/.vim/bundle/vim-trailing-whitespace,/usr/share/vim/vimfiles,/usr/share/vim/vim80,/usr/share/vim/vimfiles/after,~/.vim/after,~/.vim/bundle/Vundle.vim/after,~/.vim/bundle/nerdtree/after,~/.vim/bundle/vim-erlang-runtime/after,~/.vim/bundle/vim-erlang-tags/after,~/.vim/bundle/vim-erlang-omnicomplete/after,~/.vim/bundle/vim-erlang-skeletons/after,~/.vim/bundle/vim-trailing-whitespace/after,~/.vim/bundle/rename.vim/after
set shiftwidth=4
set tabstop=4
set window=0
" vim: set ft=vim :

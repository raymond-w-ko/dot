function ts --wraps='tmux new -s' --description 'alias ts=tmux new -s'
  tmux new -s $argv
        
end

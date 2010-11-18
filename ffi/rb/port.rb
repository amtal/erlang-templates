require 'bert'
require 'fcntl'

#STDIN.binmode
#STDIN.sync=true
#$stdin.fcntl(Fcntl::F_SETFL,Fcntl::O_NONBLOCK)

while true
    cmd = STDIN.gets
    next if cmd==nil
    STDERR.puts "received #{cmd} #{cmd.length}"
    cmd = cmd[0...cmd.length-1]
    msg = BERT.decode(cmd)
    n = msg[1]
    STDERR.puts "decoded to #{msg} with #{n}"
    #cmd = BERT.encode(t[:square, 25])
    resp = BERT.encode(t[:reply, n*n]).to_s
    STDERR.puts "responding with #{resp}"
    STDOUT.puts resp+"\n"
end

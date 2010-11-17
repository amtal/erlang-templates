require 'bert'

while true
    len = STDIN.getc.to_i
    cmd = STDIN.read len
    #cmd = t[:square, 25]
    n = BERT.decode(cmd)[1]
    resp = BERT.encode(t[:reply, n*n]).to_s
    STDOUT.putc 5
    STDOUT.puts resp
end

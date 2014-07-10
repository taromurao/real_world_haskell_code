begin
    outFile = File.open('output.txt', 'w')

    File.readlines('input.txt').each do |l|
        outFile.write(l.upcase)
    end
ensure
    outFile.close
end

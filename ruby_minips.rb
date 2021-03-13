require_relative 'print'

class RubyMinips
  class InvalidOperationError < StandardError; end
  class InvalidSyscallError < StandardError; end

  attr_reader :operation, :start_time, :file_name

  MEMORY = Hash.new(0)

  INSTRUCTIONS = []

  RFormat = Struct.new(:op, :rs, :rt, :rd, :shamt, :funct)
  IFormat = Struct.new(:op, :rs, :rt, :addr_or_val)
  JFormat = Struct.new(:op, :target_adress)

  REGISTER_HASH = Hash.new(0)

  REGISTER = [
    '$zero', '$at', '$v0', '$v1', '$a0', '$a1', '$a2', '$a3',
    '$t0', '$t1', '$t2', '$t3', '$t4', '$t5', '$t6', '$t7',
    '$s0', '$s1', '$s2', '$s3', '$s4', '$s5', '$s6', '$s7',
    '$t8', '$t9', '$k0', '$k1', '$gp', '$sp', '$fp', '$ra', 'pc'
  ].freeze

  def initialize(operation, file_name)
    @start_time = Time.now
    @operation = operation
    @file_name = file_name
  end

  def execute
    load_files(file_name)

    case operation
    when 'decode'
      decode
    when 'run'
      run
    else
      raise InvalidOperationError
    end
  end

  private

  def decode
    decode_text

    print = Print.new(REGISTER)

    INSTRUCTIONS.each do |instruction|
      if instruction.is_a?(RFormat)
        print.r_format(instruction)
      elsif instruction.is_a?(IFormat)
        print.i_format(instruction)
      else
        print.j_format(instruction)
      end
    end
  end

  def run
    prepare_base_register

    decode_text

    loop do
      actual_instruction = MEMORY[REGISTER_HASH['pc']]

      REGISTER_HASH['pc'] = "%08x" % (REGISTER_HASH['pc'].to_i(16) + 0x04)

      if actual_instruction.is_a?(RFormat)
        execute_r_format(actual_instruction)
      elsif actual_instruction.is_a?(IFormat)
        execute_i_format(actual_instruction)
      else
        execute_j_format(actual_instruction)
      end
    end
  end

  def count_instructions
    r_format = 0
    i_format = 0
    j_format = 0

    INSTRUCTIONS.each do |inst|
      if inst.is_a?(RFormat)
        r_format += 1
      elsif inst.is_a?(IFormat)
        i_format += 1
      else
        j_format += 1
      end
    end

    "Instruction Count: #{INSTRUCTIONS.size} (R: #{r_format}, I: #{i_format}, J: #{j_format})"
  end

  def prepare_base_register
    REGISTER.each do |r|
      case r
      when '$sp'
        REGISTER_HASH[r] = '7fffeffc'
      when '$gp'
        REGISTER_HASH[r] = '10008000'
      when 'pc'
        REGISTER_HASH[r] = '00400000'
      else
        REGISTER_HASH[r] = '00000000'
      end
    end
  end

  def execute_r_format(instruction)
    rs = REGISTER[instruction.rs.to_i(2)]
    rt = REGISTER[instruction.rt.to_i(2)]
    rd = REGISTER[instruction.rd.to_i(2)]
    shamt = instruction.shamt
    funct = instruction.funct

    execute_funct(funct: funct, rs: rs, rt: rt, rd: rd, shamt: shamt)
  end

  def execute_i_format(instruction)
    rs = REGISTER[instruction.rs.to_i(2)]
    rt = REGISTER[instruction.rt.to_i(2)]
    addr_or_val = instruction.addr_or_val
    op = instruction.op

    execute_opcode(op: op, rs: rs, rt: rt, addr_or_val: addr_or_val)
  end

  def execute_j_format(instruction)
    addr_or_val = instruction.target_adress
    op = instruction.op

    execute_opcode(op: op, addr_or_val: addr_or_val)
  end

  def two_complement_resp(bin)
    if bin.to_i(2) > 2**15
      bin.to_i(2) - 2**16
    else
      bin.to_i(2)
    end
  end

  def execute_opcode(op:, rs: '', rt: '', addr_or_val: '')
    case op
    when '000010' # jump
      REGISTER_HASH['pc'] = "%08x" % (addr_or_val.to_i(2) << 2)
    when '000011' # jal
      REGISTER_HASH['$ra'] = REGISTER_HASH['pc']

      REGISTER_HASH['pc'] = "%08x" % (addr_or_val.to_i(2) << 2)
    when '000100' # beq
      if REGISTER_HASH[rs] == REGISTER_HASH[rt]
        REGISTER_HASH['pc'] = "%08x" % (REGISTER_HASH['pc'].to_i(16) + (two_complement_resp(addr_or_val) << 2))
      end
    when '000101' # bne
      if REGISTER_HASH[rs] != REGISTER_HASH[rt]
        REGISTER_HASH['pc'] = "%08x" % (REGISTER_HASH['pc'].to_i(16) + (two_complement_resp(addr_or_val) << 2))
      end
    when '001000' # addi
      REGISTER_HASH[rt] = "%08x" % (REGISTER_HASH[rs].to_i(16) + two_complement_resp(addr_or_val))
    when '001100' # andi
      REGISTER_HASH[rt] = "%08x" % (REGISTER_HASH[rs].to_i(16) & two_complement_resp(addr_or_val))
    when '001001' # addiu
      REGISTER_HASH[rt] = "%08x" % (REGISTER_HASH[rs].to_i(16) + two_complement_resp(addr_or_val))
    when '001101' # ori
      REGISTER_HASH[rt] = "%08x" % (REGISTER_HASH[rs].to_i(16) | two_complement_resp(addr_or_val))
    when '001111' # lui
      REGISTER_HASH[rt] = "%08x" % (two_complement_resp(addr_or_val) << 16)
    when '100011' # lw
      temp_value = "%08x" % (REGISTER_HASH[rs].to_i(16) + two_complement_resp(addr_or_val))

      if MEMORY[temp_value] == 0
        temp_mem = "%08x" % 0
        MEMORY[temp_value] = temp_mem.dup
      else
        temp_mem = MEMORY[temp_value]
      end

      REGISTER_HASH[rt] = temp_mem
    when '101011' #sw
      MEMORY["%08x" % (REGISTER_HASH[rs].to_i(16) + two_complement_resp(addr_or_val))] = REGISTER_HASH[rt]
    else
      ''
    end
  end

  def aligned_address?(register)
    return true if register.to_i(16) % 4 == 0

    false
  end

  def format_data(data)
    temp = ""

    data.scan(/.{1,2}/).reverse_each do |str|
      temp << str
    end

    temp
  end

  def execute_funct(funct:, rs: '', rt: '', rd: '', shamt: '')
    case funct
    when '000000' # sll
      REGISTER_HASH[rd] = "%08x" % (REGISTER_HASH[rt].to_i(16) << shamt.to_i(2))
    when '000010' # srl
      REGISTER_HASH[rd] = "%08x" % (REGISTER_HASH[rt].to_i(16) >> shamt.to_i(2))
    when '001000' # jr
      REGISTER_HASH['pc'] = REGISTER_HASH[rs]
    when '001100' # syscall
      case REGISTER_HASH['$v0'].to_i(16)
      when 1
        print REGISTER_HASH['$a0'].to_i(16)
      when 4
        resp = ""
        a0 = REGISTER_HASH['$a0']
        data = []

        if !aligned_address?(a0)
          start_point = 0x10010000
          temp_a0 = a0.to_i(16)

          loop do
            if start_point > temp_a0
              start_point -= 0x04
              break
            elsif start_point == temp_a0
              break
            else
              start_point += 0x04
            end
          end

          if start_point != temp_a0
            intermed = format_data(MEMORY["%08x" % start_point])

            intermed_data = []

            intermed.scan(/.{1,2}/).each do |str|
              intermed_data << str
            end

            intermed_index = 0

            intermed_data.each_with_index do |d, i|
              break intermed_index = i if d == "00"
            end

            intermed_data.select!.each_with_index { |_, i| i > intermed_index }

            intermed_data.each do |d|
              resp += [d].pack('H*')
            end

            a0 = ("%08x" % (start_point + 0x04))
          end
        end

        loop do
          break if MEMORY[a0].eql?(0)

          data << format_data(MEMORY[a0])

          a0 = "%08x" % (a0.to_i(16) + 0x4)
        end

        data.join.scan(/.{1,2}/).each do |str|
          break if str.eql?("00")

          resp += [str].pack('H*')
        end

        print resp
      when 5
        int = STDIN.gets.chomp.to_i
        REGISTER_HASH['$v0'] = "%08x" % int
      when 10
        REGISTER_HASH['pc'] = "%08x" % (REGISTER_HASH['pc'].to_i(16) + 0x4)
        puts "\nExecution finished successfully"
        puts '-' * 30
        puts count_instructions
        puts "IPS: #{INSTRUCTIONS.size / (Time.now - start_time)}s"
        exit!
      when 11
        print "%c" % REGISTER_HASH['$a0'].to_i(16)
      else
        raise InvalidSyscallError
      end
    when '100000' # add
      REGISTER_HASH[rd] = "%08x" % (REGISTER_HASH[rs].to_i(16) + REGISTER_HASH[rt].to_i(16))
    when '100001' # addu
      REGISTER_HASH[rd] = "%08x" % (REGISTER_HASH[rs].to_i(16) + REGISTER_HASH[rt].to_i(16))

      if rd.eql?('$zero')
        REGISTER_HASH[rd] = "%08x" % 0
      end
    when '101010'
      rs_value = REGISTER_HASH[rs].to_i(16)
      rt_value =  REGISTER_HASH[rt].to_i(16)

      if rs_value < rt_value
        REGISTER_HASH[rd] = "%08x" % 1
      else
        REGISTER_HASH[rd] = "%08x" % 0
      end
    else
      ''
    end
  end

  # Utility Methods

  def to_bin_string(hexa_string)
    "%032b" % hexa_string.to_i(16)
  end

  # Decode Methods

  def decode_text
    start_point = 0x00400000

    while(MEMORY["%08x" % start_point] != 0) do
      instruction = MEMORY["%08x" % start_point]

      instruction = to_bin_string(instruction)

      classify_instruction(instruction)

      MEMORY["%08x" % start_point] = INSTRUCTIONS[-1]

      start_point = start_point + 0x04
    end
  end

  def classify_instruction(instruction)
    opcode = instruction[0..5]

    case opcode
    when '000000'
     create_r_instruction(instruction)
    when '000010', '000011'
     create_j_instruction(instruction)
    else
     create_i_instruction(instruction)
    end
  end

  def create_r_instruction(instruction)
    INSTRUCTIONS << RFormat.new(
        instruction[0..5],
        instruction[6..10],
        instruction[11..15],
        instruction[16..20],
        instruction[21..25],
        instruction[26..31]
      )
    end

  def create_i_instruction(instruction)
    INSTRUCTIONS << IFormat.new(
      instruction[0..5],
      instruction[6..10],
      instruction[11..15],
      instruction[16..31]
    )
  end

  def create_j_instruction(instruction)
    INSTRUCTIONS << JFormat.new(
      instruction[0..5],
      instruction[6..31]
    )
  end

  # Load Methods

  def load_files(file_name)
    load_text(file_name + '.text')
    load_data(file_name + '.data')
  end

  def load_text(file_name)
    instructions = File.binread(file_name)

    unpacked_instructions = instructions.unpack('H*')[0]

    # operação retirada do stack overflow
    formatted_instructions = [unpacked_instructions].pack('H*').unpack('N*').pack('V*').unpack('H*')

    sliced_instructions = slice_instructions(formatted_instructions)

    load_in_memory(sliced_instructions, 'text')
  end

  def load_data(file_name)
    file = File.open(file_name)

    data = []
    prev = nil

    loop do
      aux = file.read(4)

      break if aux.nil?

      data << aux.unpack('h*').first

      if prev == nil
        prev = data[-1]
      elsif data[-1] == '00000000' && data[-2] == '00000000'
        break
      end
    end

    file.close

    data.map!(&:reverse)

    load_in_memory(data, 'data')
  end

  def slice_instructions(instructions)
    sliced_instructions = []

    # slice in 4bytes words
    (0...instructions[0].size).step(8).each do |i|
      sliced_instructions << instructions[0][i..i+7]
    end

    # remove null elements
    sliced_instructions.reject!.each_with_index do |e, i|
      sliced_instructions[i] == '00000000' && sliced_instructions[i+1] == '00000000'
    end

    sliced_instructions
  end

  def load_in_memory(words, type)
    initial_position = type == 'text' ? 0x00400000 : 0x10010000

    words.each do |word|
      MEMORY["%08x" % initial_position] = word

      initial_position = initial_position + 0x04
    end
  end
end

RubyMinips.new(ARGV[0], ARGV[1]).execute

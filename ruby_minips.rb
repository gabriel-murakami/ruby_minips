require_relative 'print'
require 'rubygems'
require 'float-formats'
include Flt

class RubyMinips
  class InvalidOperationError < StandardError; end
  class InvalidSyscallError < StandardError; end

  attr_reader :operation, :start_time, :file_name, :r_format_count, :j_format_count, :i_format_count, :fr_format_count, :fi_format_count
  attr_reader :monocycle_time, :pipelined_time, :monocycle_cycles, :pipelined_cycles, :condition_bit, :cache_type, :cache_hit, :cache_miss

  MEMORY = Hash.new(nil)

  INSTRUCTIONS = []

  CACHE = []
  Line = Struct.new(:id, :elements)
  Element = Struct.new(:tag, :data)

  RFormat = Struct.new(:addr, :hex, :op, :rs, :rt, :rd, :shamt, :funct, :nop?)
  FRFormat = Struct.new(:addr, :hex, :op, :fmt, :ft, :fs, :fd, :funct)
  IFormat = Struct.new(:addr, :hex, :op, :rs, :rt, :addr_or_val)
  FIFormat = Struct.new(:addr, :hex, :op, :fmt, :ft, :addr_or_val)
  JFormat = Struct.new(:addr, :hex, :op, :target_adress)

  REGISTER_HASH = Hash.new(0)

  FLOAT_REGISTER_HASH = Hash.new(0)

  REGISTER = [
    '$zero', '$at', '$v0', '$v1', '$a0', '$a1', '$a2', '$a3',
    '$t0', '$t1', '$t2', '$t3', '$t4', '$t5', '$t6', '$t7',
    '$s0', '$s1', '$s2', '$s3', '$s4', '$s5', '$s6', '$s7',
    '$t8', '$t9', '$k0', '$k1', '$gp', '$sp', '$fp', '$ra', 'pc'
  ].freeze

  FLOAT_REGISTER = [
    "$f0", "$f1", "$f2", "$f3", "$f4", "$f5", "$f6", "$f7",
    "$f8", "$f9", "$f10", "$f11", "$f12", "$f13", "$f14", "$f15",
    "$f16", "$f17", "$f18", "$f19", "$f20", "$f21", "$f22", "$f23",
    "$f24", "$f25", "$f26", "$f27", "$f28", "$f29", "$f30", "$f31", 'hi', 'lo'
  ].freeze

  def initialize(operation, cache_type, file_name)
    @start_time = Time.now
    @condition_bit = false
    @operation = operation
    @file_name = file_name
    @cache_type = cache_type
    @cache_hit_ram = 0
    @cache_miss_ram = 0
    @cache_hit_l1 = 0
    @cache_miss_l1 = 0
    @r_format_count = 0
    @i_format_count = 0
    @j_format_count = 0
    @fr_format_count = 0
    @fi_format_count = 0
    @monocycle_time = 0.0
    @pipelined_time = 0.0
    @monocycle_cycles = 0
    @pipelined_cycles = 0
  end

  def execute
    load_files(file_name)

    case operation
    when 'decode'
      decode
    when 'run'
      run
    when 'trace'
      File.open('minips.trace', 'w') {|file| file.truncate(0) }
      run
    else
      raise InvalidOperationError
    end
  end

  private

  def decode
    decode_all_instructions

    print = Print.new(REGISTER, FLOAT_REGISTER)

    INSTRUCTIONS.each do |instruction|
      to_print = if instruction.is_a?(RFormat)
        print.r_format(instruction)
      elsif instruction.is_a?(IFormat)
        print.i_format(instruction)
      elsif instruction.is_a?(FRFormat)
        print.fr_format(instruction)
      elsif instruction.is_a?(FIFormat)
        print.fi_format(instruction)
      else
        print.j_format(instruction)
      end

      puts "0x#{instruction.addr}:       0x#{instruction.hex}       #{to_print}"
    end
  end

  def run
    prepare_base_register(REGISTER, REGISTER_HASH)
    prepare_base_register(FLOAT_REGISTER, FLOAT_REGISTER_HASH)

    loop do
      actual_instruction = decode_single_instruction(read_memory(REGISTER_HASH['pc']))

      if operation.eql?('trace')
        append_trace(actual_instruction, REGISTER_HASH['pc'])
      end

      REGISTER_HASH['pc'] = REGISTER_HASH['pc'] + 4

      run_single_instruction(actual_instruction)
    end
  end

  def append_trace(actual_instruction, addr)
    type = actual_instruction.class.to_s.split('::')[1].gsub('Format', '')
    line = addr / 32

    File.write('minips.trace', "#{type} 0x#{"%08x" % addr} (line# 0x#{"%08x" % line})\n", mode: 'a')
  end

  def run_single_instruction(actual_instruction)
    if actual_instruction.is_a?(RFormat)
      @r_format_count += 1
      execute_r_format(actual_instruction)
    elsif actual_instruction.is_a?(IFormat)
      @i_format_count += 1
      execute_i_format(actual_instruction)
    elsif actual_instruction.is_a?(FRFormat)
      @fr_format_count += 1
      execute_fr_format(actual_instruction)
    elsif actual_instruction.is_a?(FIFormat)
      @fi_format_count += 1
      execute_fi_format(actual_instruction)
    else
      @j_format_count += 1
      execute_j_format(actual_instruction)
    end
  end

  def count_instructions
    all_format_count = @r_format_count + @i_format_count + @j_format_count + @fr_format_count + @fi_format_count

    puts "Instruction Count: #{all_format_count} (R: #{@r_format_count}, I: #{@i_format_count}, J: #{@j_format_count}, FR: #{@fr_format_count}, FI: #{@fi_format_count})"
    puts "IPS: #{all_format_count / (Time.now - start_time)}s"
  end

  def count_time
    all_format_count = @r_format_count + @i_format_count + @j_format_count + @fr_format_count + @fi_format_count

    mono_ipc = all_format_count.to_f / @monocycle_cycles
    mono_time = @monocycle_cycles.to_f / (8.4672*(10**6))
    mono_mips = all_format_count.to_f / (mono_time*(10**6))

    pipe_ipc = all_format_count.to_f / (@monocycle_cycles + 4)
    pipe_time = (@monocycle_cycles.to_f + 4.0) / (33.8688*(10**6))
    pipe_mips = all_format_count.to_f / (pipe_time*(10**6))

    puts "\nSimulated execution times for:\n" + ("-" * 30)
    puts "Monocyle"
    puts "  Cycles: #{@monocycle_cycles}"
    puts "  Frequency: 8.4672 MHz"
    puts "  Estimated execution time: #{"%0.05f" % mono_time}"
    puts "  IPC: #{"%0.2f" % mono_ipc}"
    puts "  MIPS: #{"%0.2f" % mono_mips}"
    puts "Pipelined"
    puts "  Cycles: #{@monocycle_cycles + 4}"
    puts "  Frequency: 33.8688 MHz"
    puts "  Estimated execution time: #{"%0.05f" % @pipelined_time}"
    puts "  IPC: #{"%0.2f" % pipe_ipc} "
    puts "  MIPS: #{"%0.2f" % pipe_mips}"
  end

  def cache_hits_and_misses
    puts "\nMemory Information\n" + ("-" * 30)
    printf("%-6s %-10s %-10s %-10s %-10s\n", 'Level', 'Hits', 'Misses', 'Total', 'Miss Rate')
    printf("%-6s %-10s %-10s %-10s %-10s\n", '-'*6, '-'*10, '-'*10, '-'*10, '-'*10)

    case @cache_type
    when '1'
      total = @cache_hit_ram + @cache_miss_ram
      miss_rate = @cache_miss_ram / total

      printf("%6s %10s %10s %10s %10s\n", 'RAM', @cache_hit_ram, @cache_miss_ram, total, "#{miss_rate} %")
    when '2'
      total_ram = @cache_hit_ram + @cache_miss_ram
      miss_rate_ram = ((@cache_miss_ram / total_ram.to_f) * 100).round(2)
      total_l1 = @cache_hit_l1 + @cache_miss_l1
      miss_rate_l1 = ((@cache_miss_l1 / total_l1.to_f) * 100).round(2)

      printf("%6s %10s %10s %10s %10s\n", 'L1', @cache_hit_l1, @cache_miss_l1, total_l1, "#{miss_rate_l1} %")
      printf("%6s %10s %10s %10s %10s\n", 'RAM', @cache_hit_ram, @cache_miss_ram, total_ram, "#{miss_rate_ram} %")
    when '3'
    when '4'
    when '5'
    when '6'
    end
  end

  def prepare_base_register(register, hash)
    register.each do |r|
      case r
      when '$sp'
        hash[r] = 0x7fffeffc
      when '$gp'
        hash[r] = 0x10008000
      when 'pc'
        hash[r] = 0x00400000
      else
        hash[r] = 0x00000000
      end
    end
  end

  # cache methods

  def on_cache?(addr)
    case @cache_type
    when '1'
      false
    when '2'
      CACHE.each do |line|
        line.elements.each do |e|
          if e.tag.eql?(addr)
            return true
          end
        end
      end

      false
    when '3'
    when '4'
    when '5'
    when '6'
    end
  end

  def load_cache_l1(addr)
    line_id = addr/32
    all_line_elements = MEMORY.map { |k, v| [k,v] if (k/32) == (addr/32) }

    all_line_elements = all_line_elements.reject { |e| e.nil? }

    all_line_elements.map! do |le|
      Element.new(le[0], le[1])
    end

    @cache_miss_l1 += 1
    @cache_hit_ram += 1
    @monocycle_cycles += 100

    CACHE << Line.new(line_id, all_line_elements)

    MEMORY[addr]
  end

  def read_memory(addr)
    case @cache_type
    when '1'
      @cache_hit_ram += 1
      @monocycle_cycles += 101
      MEMORY[addr]
    when '2'
      line_id = (addr / 32)
      data_resp = nil

      CACHE.each do |line|
        if line.id.eql?(line_id)
          line.elements.each do |e|
            if addr.eql?(e.tag)
              @cache_hit_l1 += 1
              @monocycle_cycles += 1
              data_resp = e.data
              return data_resp
            end
          end
        end
      end

      if data_resp.nil?
        data_resp = load_cache_l1(addr)
      end

      data_resp
    when '3'
    when '4'
    when '5'
    when '6'
    end
  end

  def write_memory(addr, data)
    if operation.eql?('trace')
      File.write('minips.trace', "W 0x#{"%08x" % addr} (line# 0x#{"%08x" % (addr/32)})\n", mode: 'a')
    end

    case @cache_type
    when '1'
      @cache_hit_ram += 1
      @monocycle_cycles += 101
      MEMORY[addr] = data
    when '2'
      if on_cache?(addr)
        @cache_hit_l1 += 1
        MEMORY[addr] = read_memory(addr)
      else
        @cache_miss_l1 += 1
        MEMORY[addr] = data
      end
    when '3'
    when '4'
    when '5'
    when '6'
    end
  end

  # Execution Methods

  def execute_r_format(instruction)
    rs = REGISTER[instruction.rs]
    rt = REGISTER[instruction.rt]
    rd = REGISTER[instruction.rd]
    shamt = instruction.shamt
    funct = instruction.funct

    execute_funct(instruction, funct: funct, rs: rs, rt: rt, rd: rd, shamt: shamt)
  end

  def execute_i_format(instruction)
    rs = REGISTER[instruction.rs]
    rt = REGISTER[instruction.rt]
    addr_or_val = instruction.addr_or_val
    op = instruction.op

    execute_opcode(instruction, op: op, rs: rs, rt: rt, addr_or_val: addr_or_val)
  end

  def execute_j_format(instruction)
    addr_or_val = instruction.target_adress
    op = instruction.op

    execute_opcode(instruction, op: op, addr_or_val: addr_or_val)
  end

  def execute_fr_format(instruction)
    fmt = instruction.fmt
    ft = FLOAT_REGISTER[instruction.ft]
    fs = FLOAT_REGISTER[instruction.fs]
    fd = FLOAT_REGISTER[instruction.fd]
    funct = instruction.funct

    execute_fr(instruction, funct: funct, fmt: fmt, ft: ft, fs: fs, fd: fd)
  end

  def execute_fi_format(instruction)
    fmt = instruction.fmt
    ft = FLOAT_REGISTER[instruction.ft]
    addr_or_val = instruction.addr_or_val

    execute_fi(instruction, fmt: fmt, ft: ft, addr_or_val: addr_or_val)
  end

  # solução retirada do Stack Overflow
  def two_complement_resp(bin)
    if bin > 2**15
      bin - 2**16
    else
      bin
    end
  end

  def two_complement_reg(bin)
    if bin > 2**31
      bin - 2**32
    else
      bin
    end
  end

  def bin_to_float(sign, exp, frac, bias)
    sum_frac = 0.0

    frac = frac.to_s(2).split('').map(&:to_i)

    (0...frac.length).each do |i|
      sum_frac += (frac[i] * (2**(-(i+1))))
    end

    (((-1)**sign)*(1+sum_frac)*(2**(exp-bias)))
  end

  def bin32_to_float(bin)
    sign = bin[31]
    exponent = bin[23..30]
    fraction = bin[0..22]

    bin_to_float(sign, exponent, fraction, 127)
  end

  def bin64_to_float(bin)
    sign = bin[63]
    exponent = bin[52..62]
    fraction = bin[0..51]

    bin_to_float(sign, exponent, fraction, 1023)
  end

  def execute_fi(instruction, fmt: '', ft: '', addr_or_val: '')
    case ft
    when 0b00000
      aux = REGISTER_HASH['pc']

      if !@condition_bit
        REGISTER_HASH['pc'] = REGISTER_HASH['pc'] + (two_complement_resp(addr_or_val) << 2)
        run_single_instruction(decode_single_instruction(read_memory(aux)))
      end
    when 0b00001 # bc1t
      aux = REGISTER_HASH['pc']

      if @condition_bit
        REGISTER_HASH['pc'] = REGISTER_HASH['pc'] + (two_complement_resp(addr_or_val) << 2)
        run_single_instruction(decode_single_instruction(read_memory(aux)))
      end
    end
  end

  def execute_fr(instruction, funct: '', fmt: '', ft: '', fs: '', fd: '')
    case fmt
    when 0b00000 # mfc1
      rt = REGISTER[instruction.ft]
      REGISTER_HASH[rt] = FLOAT_REGISTER_HASH[fs]
    when 0b10000
      case funct
      when 0b000000 # add.s
        fs_value = ["%08x" % FLOAT_REGISTER_HASH[fs]].pack('H*').unpack('g').first
        ft_value = ["%08x" % FLOAT_REGISTER_HASH[ft]].pack('H*').unpack('g').first

        fd_value = fs_value + ft_value

        FLOAT_REGISTER_HASH[fd] = [fd_value.to_f].pack('g').unpack('H*').first.to_i(16)
      when 0b000001
        fs_value = ["%08x" % FLOAT_REGISTER_HASH[fs]].pack('H*').unpack('g').first
        ft_value = ["%08x" % FLOAT_REGISTER_HASH[ft]].pack('H*').unpack('g').first

        fd_value = fs_value - ft_value

        FLOAT_REGISTER_HASH[fd] = [fd_value.to_f].pack('g').unpack('H*').first.to_i(16)
      when 0b000010 # mul.s
        fs_value = ["%08x" % FLOAT_REGISTER_HASH[fs]].pack('H*').unpack('g').first
        ft_value = ["%08x" % FLOAT_REGISTER_HASH[ft]].pack('H*').unpack('g').first

        fd_value = fs_value * ft_value

        FLOAT_REGISTER_HASH[fd] = [fd_value.to_f].pack('g').unpack('H*').first.to_i(16)
      when 0b000110 # mov.s
        FLOAT_REGISTER_HASH[fd] = FLOAT_REGISTER_HASH[fs]
      when 0b111100 # c.lt.s
        if FLOAT_REGISTER_HASH[fs] < FLOAT_REGISTER_HASH[ft]
          @condition_bit = true
        end
      end
    when 0b10001
      case funct
      when 0b000000 # add.d
        fs_value_1 = "%08x" % FLOAT_REGISTER_HASH[fs]
        fs_value_2 = "%08x" % FLOAT_REGISTER_HASH[FLOAT_REGISTER[instruction.fs + 1]]

        ft_value_1 = "%08x" % FLOAT_REGISTER_HASH[ft]
        ft_value_2 = "%08x" % FLOAT_REGISTER_HASH[FLOAT_REGISTER[instruction.ft + 1]]

        fs_value = [(fs_value_2 + fs_value_1)].pack('H*').unpack('G').first
        ft_value = [(ft_value_2 + ft_value_1)].pack('H*').unpack('G').first

        fd_value = fs_value + ft_value

        FLOAT_REGISTER_HASH[fd] = [fd_value.to_f].pack('G').unpack('H*').first.to_i(16)
      when 0b000010 # mul.d
        fs_value_1 = "%08x" % FLOAT_REGISTER_HASH[fs]
        fs_value_2 = "%08x" % FLOAT_REGISTER_HASH[FLOAT_REGISTER[instruction.fs + 1]]

        ft_value_1 = "%08x" % FLOAT_REGISTER_HASH[ft]
        ft_value_2 = "%08x" % FLOAT_REGISTER_HASH[FLOAT_REGISTER[instruction.ft + 1]]

        fs_value = [(fs_value_2 + fs_value_1)].pack('H*').unpack('G').first
        ft_value = [(ft_value_2 + ft_value_1)].pack('H*').unpack('G').first

        fd_value = fs_value * ft_value

        FLOAT_REGISTER_HASH[fd] = [fd_value.to_f].pack('G').unpack('H*').first.to_i(16)
      when 0b000011 # div.d
        fs_value_1 = "%08x" % FLOAT_REGISTER_HASH[fs]
        fs_value_2 = "%08x" % FLOAT_REGISTER_HASH[FLOAT_REGISTER[instruction.fs + 1]]

        ft_value_1 = "%08x" % FLOAT_REGISTER_HASH[ft]
        ft_value_2 = "%08x" % FLOAT_REGISTER_HASH[FLOAT_REGISTER[instruction.ft + 1]]

        fs_value = [(fs_value_2 + fs_value_1)].pack('H*').unpack('G').first
        ft_value = [(ft_value_2 + ft_value_1)].pack('H*').unpack('G').first

        fd_value = fs_value / ft_value

        FLOAT_REGISTER_HASH[fd] = [fd_value.to_f].pack('G').unpack('H*').first.to_i(16)
      when 0b000110 # mov.d
        fd1 = fd
        fd2 = FLOAT_REGISTER[instruction.fd + 1]

        fs1 = fs
        fs2 = FLOAT_REGISTER[instruction.fs + 1]

        FLOAT_REGISTER_HASH[fd1] = FLOAT_REGISTER_HASH[fs1]
        FLOAT_REGISTER_HASH[fd2] = FLOAT_REGISTER_HASH[fs2]
      when 0b100000 # cvt.s.d
        fs_value_1 = "%08x" % FLOAT_REGISTER_HASH[fs]
        fs_value_2 = "%08x" % FLOAT_REGISTER_HASH[FLOAT_REGISTER[instruction.fs + 1]]

        fs_value = [(fs_value_2 + fs_value_1)].pack('H*').unpack('G').pack('g').unpack('H*').first.to_i(16)

        FLOAT_REGISTER_HASH[fd] = fs_value
      end
    when 0b10100 # cvt.d.w
      converted_fs = [FLOAT_REGISTER_HASH[fs]].pack('G').unpack('H*').first.to_i(16)

      FLOAT_REGISTER_HASH[fd] = converted_fs[0..31]
      FLOAT_REGISTER_HASH[FLOAT_REGISTER[instruction.fd + 1]] = converted_fs[32..63]
    when 0b00100 # mtc1
      rt = REGISTER[instruction.ft]
      FLOAT_REGISTER_HASH[fs] = REGISTER_HASH[rt]
    end
  end

  def execute_opcode(instruction, op:, rs: '', rt: '', addr_or_val: '')
    case op
    when 0b000001 # bgez
      aux = REGISTER_HASH['pc']

      if REGISTER_HASH[rs] >= 0
        REGISTER_HASH['pc'] = REGISTER_HASH['pc'] + (two_complement_resp(addr_or_val) << 2)
        run_single_instruction(decode_single_instruction(read_memory(aux)))
      end
    when 0b000010 # jump
      aux = REGISTER_HASH['pc']

      REGISTER_HASH['pc'] = addr_or_val << 2

      run_single_instruction(decode_single_instruction(read_memory(aux)))
    when 0b000011 # jal
      aux = REGISTER_HASH['pc']

      REGISTER_HASH['$ra'] = REGISTER_HASH['pc'] + 4

      REGISTER_HASH['pc'] = addr_or_val << 2

      run_single_instruction(decode_single_instruction(read_memory(aux)))
    when 0b000100 # beq
      aux = REGISTER_HASH['pc']

      if REGISTER_HASH[rs] == REGISTER_HASH[rt]
        REGISTER_HASH['pc'] = REGISTER_HASH['pc'] + (two_complement_resp(addr_or_val) << 2)
        run_single_instruction(decode_single_instruction(read_memory(aux)))
      end
    when 0b000101 # bne
      aux = REGISTER_HASH['pc']

      if REGISTER_HASH[rs] != REGISTER_HASH[rt]
        REGISTER_HASH['pc'] = REGISTER_HASH['pc'] + (two_complement_resp(addr_or_val) << 2)
        run_single_instruction(decode_single_instruction(read_memory(aux)))
      end
    when 0b001000 # addi
      REGISTER_HASH[rt] = REGISTER_HASH[rs] + two_complement_resp(addr_or_val)
    when 0b001010
      if REGISTER_HASH[rs] < addr_or_val
        REGISTER_HASH[rt] = 1
      else
        REGISTER_HASH[rt] = 0
      end
    when 0b001100 # andi
      REGISTER_HASH[rt] = REGISTER_HASH[rs] & two_complement_resp(addr_or_val)
    when 0b001001 # addiu
      REGISTER_HASH[rt] = REGISTER_HASH[rs] + two_complement_resp(addr_or_val)
    when 0b001101 # ori
      REGISTER_HASH[rt] = REGISTER_HASH[rs] | two_complement_resp(addr_or_val)
    when 0b001111 # lui
      REGISTER_HASH[rt] = two_complement_resp(addr_or_val) << 16
    when 0b100000 # lb
      temp_value = REGISTER_HASH[rs] + two_complement_resp(addr_or_val)

      if MEMORY[temp_value].nil?
        temp_mem = 0
        MEMORY[temp_value] = temp_mem
      else
        temp_mem = read_memory(temp_value)
      end

      REGISTER_HASH[rt] = temp_mem
    when 0b100011 # lw
      temp_value = REGISTER_HASH[rs] + two_complement_resp(addr_or_val)

      if MEMORY[temp_value].nil?
        temp_mem = 0
        MEMORY[temp_value] = temp_mem
      else
        temp_mem = read_memory(temp_value)
      end

      REGISTER_HASH[rt] = temp_mem
    when 0b101011 #sw
      write_memory((REGISTER_HASH[rs] + two_complement_resp(addr_or_val)), REGISTER_HASH[rt])
    when 0b110001 #lwc1
      temp_value = REGISTER_HASH[rs] + two_complement_resp(addr_or_val)

      if MEMORY[temp_value].nil?
        temp_mem = 0
        MEMORY[temp_value] = temp_mem
      else
        temp_mem = MEMORY[temp_value]
      end

      ft = FLOAT_REGISTER[instruction.rt]
      FLOAT_REGISTER_HASH[ft] = temp_mem
    when 0b110101 #ldc1
      temp_value_1 = REGISTER_HASH[rs] + two_complement_resp(addr_or_val)
      temp_value_2 = temp_value_1 + 4

      if MEMORY[temp_value_1].nil?
        temp_mem_1 = 0
        temp_mem_2 = 0
        MEMORY[temp_value_1] = temp_mem_1
        MEMORY[temp_value_2] = temp_mem_2
      else
        temp_mem_1 = MEMORY[temp_value_1]
        temp_mem_2 = MEMORY[temp_value_2]
      end

      ft1 = FLOAT_REGISTER[instruction.rt]
      ft2 = FLOAT_REGISTER[instruction.rt + 1]

      FLOAT_REGISTER_HASH[ft1] = temp_mem_1
      FLOAT_REGISTER_HASH[ft2] = temp_mem_2
    when 0b111001 # swc1
      MEMORY[REGISTER_HASH[rs] + two_complement_resp(addr_or_val)] = FLOAT_REGISTER_HASH[rt]
    else
      ''
    end
  end

  def format_data(data)
    temp = ""

    data = "%08x" % data

    data.scan(/.{1,2}/).reverse_each do |str|
      temp << str
    end

    temp
  end

  def aligned_address?(register)
    return true if register % 4 == 0

    false
  end

  def execute_funct(instruction, funct:, rs: '', rt: '', rd: '', shamt: '')
    case funct
    when 0b000000 # sll
      REGISTER_HASH[rd] = REGISTER_HASH[rt] << shamt
    when 0b000010 # srl
      REGISTER_HASH[rd] = REGISTER_HASH[rt] >> shamt
    when 0b000011 # sra
      sign = REGISTER_HASH[rt][31]
      temp = REGISTER_HASH[rt] >> shamt

      REGISTER_HASH[rd] = ((temp) + sign*(2**31))
    when 0b001000 # jr
      aux = REGISTER_HASH['pc']

      REGISTER_HASH['pc'] = REGISTER_HASH[rs]

      run_single_instruction(decode_single_instruction(read_memory(aux)))
    when 0b001001 # jalr
      aux = REGISTER_HASH['pc']

      temp = REGISTER_HASH[rs]
      REGISTER_HASH[rd] = REGISTER_HASH['pc'] + 4
      REGISTER_HASH['pc'] = temp

      run_single_instruction(decode_single_instruction(read_memory(aux)))
    when 0b001100 # syscall
      case REGISTER_HASH['$v0']
      when 1
        print REGISTER_HASH['$a0']
      when 2
        print IEEE_binary32(["%08x" % FLOAT_REGISTER_HASH['$f12']].pack('H*').unpack('g').first).to_text
      when 3
        aux = ("%08x" % FLOAT_REGISTER_HASH['$f13']) + ("%08x" % FLOAT_REGISTER_HASH['$f12'])

        print IEEE_binary64([aux].pack('H*').unpack('G').first).to_text
      when 4
        resp = ""
        a0 = REGISTER_HASH['$a0']
        data = []

        if !aligned_address?(a0)
          start_point = 0x10010000
          temp_a0 = a0

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
            intermed = format_data(MEMORY[start_point])

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

            a0 = start_point + 4
          end
        end

        loop do
          break if MEMORY[a0].nil?

          data << format_data(read_memory(a0))

          a0 = a0 + 4
        end

        data.join.scan(/.{1,2}/).each do |str|
          break if str.eql?("00")

          resp += [str].pack('H*')
        end

        print resp
      when 5
        REGISTER_HASH['$v0'] = STDIN.gets.chomp.to_i
      when 6
        float = STDIN.gets.chomp.to_f

        FLOAT_REGISTER_HASH['$f0'] = [float].pack('g').unpack('H*').first.to_i(16)
      when 7
        float = STDIN.gets.chomp.to_f

        aux = [float].pack('G').unpack('H*').first.to_i(16)

        FLOAT_REGISTER_HASH['$f0'] = aux[0..31]
        FLOAT_REGISTER_HASH['$f1'] = aux[32..64]
      when 10
        REGISTER_HASH['pc'] = REGISTER_HASH['pc'] + 4
        puts "\nExecution finished successfully"
        puts '-' * 30
        count_instructions
        count_time
        cache_hits_and_misses
        exit!
      when 11
        print "%c" % REGISTER_HASH['$a0']
      else
        raise InvalidSyscallError
      end
    when 0b010000 # mfhi
      REGISTER_HASH[rd] = FLOAT_REGISTER_HASH['hi']
    when 0b010010 # mflo
      REGISTER_HASH[rd] = FLOAT_REGISTER_HASH['lo']
    when 0b011000 # mult
      rs_value = two_complement_reg(REGISTER_HASH[rs])
      rt_value = two_complement_reg(REGISTER_HASH[rt])

      mult_value = (rs_value * rt_value)

      FLOAT_REGISTER_HASH['hi'] = mult_value[16..31]
      FLOAT_REGISTER_HASH['lo'] = mult_value[0..15]
    when 0b011010 # div
      rs_value = two_complement_reg(REGISTER_HASH[rs])
      rt_value = two_complement_reg(REGISTER_HASH[rt])

      q = rs_value / rt_value
      r = rs_value % rt_value

      FLOAT_REGISTER_HASH['hi'] = r
      FLOAT_REGISTER_HASH['lo'] = q
    when 0b100000 # add
      if !rd.eql?('$zero')
        REGISTER_HASH[rd] = two_complement_reg(REGISTER_HASH[rs]) + two_complement_reg(REGISTER_HASH[rt])
      end
    when 0b100001 # addu
      if !rd.eql?('$zero')
        REGISTER_HASH[rd] = two_complement_reg(REGISTER_HASH[rs]) + two_complement_reg(REGISTER_HASH[rt])
      end
    when 0b100011 # subu
      if !rd.eql?('$zero')
        REGISTER_HASH[rd] = two_complement_reg(REGISTER_HASH[rs]) - two_complement_reg(REGISTER_HASH[rt])
      end
    when 0b100011
      if !rd.eql?('$zero')
        REGISTER_HASH[rd] = REGISTER_HASH[rs] - REGISTER_HASH[rt]
      end
    when 0b100100 # and
      REGISTER_HASH[rd] = two_complement_reg(REGISTER_HASH[rs]) & two_complement_reg(REGISTER_HASH[rt])
    when 0b100101 # or
      REGISTER_HASH[rd] = two_complement_reg(REGISTER_HASH[rs]) | two_complement_reg(REGISTER_HASH[rt])
    when 0b100110 # xor
      REGISTER_HASH[rd] = two_complement_reg(REGISTER_HASH[rs]) ^ two_complement_reg(REGISTER_HASH[rt])
    when 0b101010 # slt
      rs_value = two_complement_reg(REGISTER_HASH[rs])
      rt_value = two_complement_reg(REGISTER_HASH[rt])

      if rs_value < rt_value
        REGISTER_HASH[rd] = 1
      else
        REGISTER_HASH[rd] = 0
      end
    when 0b101011 # sltu
      rs_value = two_complement_reg(REGISTER_HASH[rs])
      rt_value = two_complement_reg(REGISTER_HASH[rt])

      if rs_value < rt_value
        REGISTER_HASH[rd] = 1
      else
        REGISTER_HASH[rd] = 0
      end
    else
      ''
    end
  end

  # Decode Methods

  def decode_all_instructions
    start_point = 0x00400000

    while(MEMORY[start_point] != nil) do
      instruction = MEMORY[start_point]

      INSTRUCTIONS << classify_instruction(instruction, addr: start_point)

      MEMORY[start_point] = INSTRUCTIONS[-1]

      start_point = start_point + 0x04
    end
  end

  def decode_single_instruction(instruction)
    classify_instruction(instruction)
  end

  def classify_instruction(instruction, addr: 0)
    opcode = instruction[26..31]
    fmt = instruction[21..25]

    case opcode
    when 0b000000
     create_r_instruction(instruction, addr: addr)
    when 0b010001
      if fmt.eql?(0b01000)
        create_fi_instruction(instruction, addr: addr)
      else
        create_fr_instruction(instruction, addr: addr)
      end
    when 0b000010, 0b000011
     create_j_instruction(instruction, addr: addr)
    else
     create_i_instruction(instruction, addr: addr)
    end
  end

  def create_fr_instruction(instruction, addr: 0)
    FRFormat.new(
      "%08x" % addr,
      "%08x" % instruction,
      instruction[26..31],
      instruction[21..25],
      instruction[16..20],
      instruction[11..15],
      instruction[6..10],
      instruction[0..5]
    )
  end

  def create_r_instruction(instruction, addr: 0)
    RFormat.new(
      "%08x" % addr,
      "%08x" % instruction,
      instruction[26..31],
      instruction[21..25],
      instruction[16..20],
      instruction[11..15],
      instruction[6..10],
      instruction[0..5],
      (instruction.zero? ? true : false)
    )
  end

  def create_fi_instruction(instruction, addr: 0)
    FIFormat.new(
      "%08x" % addr,
      "%08x" % instruction,
      instruction[26..31],
      instruction[21..25],
      instruction[16..20],
      instruction[0..15]
    )
  end

  def create_i_instruction(instruction, addr: 0)
    IFormat.new(
      "%08x" % addr,
      "%08x" % instruction,
      instruction[26..31],
      instruction[21..25],
      instruction[16..20],
      instruction[0..15]
    )
  end

  def create_j_instruction(instruction, addr: 0)
    JFormat.new(
      "%08x" % addr,
      "%08x" % instruction,
      instruction[26..31],
      instruction[0..25]
    )
  end

  # Load Methods

  def load_files(file_name)
    read_file("#{file_name}.text", 'text')
    read_file("#{file_name}.data", 'data')
    read_file("#{file_name}.rodata", 'rodata')
  end

  def read_file(file_name, type)
    file = File.open(file_name) if File.file?(file_name)

    return unless file

    data = []
    prev = nil

    loop do
      aux = file.read(4)

      break if aux.nil?

      data << aux.unpack('L*').first

      if prev == nil
        prev = data[-1]
      elsif data[-1].zero? && data[-2].zero? && data[-3].zero? && data[-4].zero?
        break
      end
    end

    file.close

    load_in_memory(data, type)
  end

  def load_in_memory(words, type)
    case type
    when 'text'
      initial_position = 0x00400000
    when 'data'
      initial_position = 0x10010000
    else
      initial_position = 0x00800000
    end

    words.each do |word|
      MEMORY[initial_position] = word

      initial_position = initial_position + 0x04
    end
  end
end

CACHE_OPTIONS = (1..6).map(&:to_s)

if CACHE_OPTIONS.include?(ARGV[1])
  RubyMinips.new(ARGV[0], ARGV[1], ARGV[2]).execute
else
  RubyMinips.new(ARGV[0], '1', ARGV[1]).execute
end

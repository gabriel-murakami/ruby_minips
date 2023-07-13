class Print
  attr_reader :register, :float_register

  def initialize(register, float_register)
    @register = register
    @float_register = float_register
  end

  def r_format(instruction)
    rs = register[instruction.rs]
    rt = register[instruction.rt]
    rd = register[instruction.rd]
    shamt = instruction.shamt
    funct = instruction.funct

    funct_to_print(instruction, funct: funct, rs: rs, rt: rt, rd: rd, shamt: shamt)
  end

  def fr_format(instruction)
    fmt = instruction.fmt
    ft = float_register[instruction.ft]
    fs = float_register[instruction.fs]
    fd = float_register[instruction.fd]
    funct = instruction.funct

    fr_to_print(instruction, funct: funct, fmt: fmt, ft: ft, fs: fs, fd: fd)
  end

  def i_format(instruction)
    rs = register[instruction.rs]
    rt = register[instruction.rt]
    addr_or_val = instruction.addr_or_val
    op = instruction.op

    opcode_to_print(instruction, op: op, rs: rs, rt: rt, addr_or_val: addr_or_val)
  end

  def fi_format(instruction)
    fmt = instruction.fmt
    ft = instruction.ft
    addr_or_val = instruction.addr_or_val

    fi_to_print(instruction, fmt: fmt, ft: ft, addr_or_val: addr_or_val)
  end

  def j_format(instruction)
    addr_or_val = instruction.target_adress
    op = instruction.op

    opcode_to_print(instruction, op: op, addr_or_val: addr_or_val)
  end

  private

  def two_complement_resp(bin)
    if bin >= 2**15
      bin - 2**16
    else
      bin
    end
  end

  def fi_to_print(instruction, fmt: '', ft: '', addr_or_val: '')
    case ft
    when 0b00000
      "bc1f #{float_register[instruction.ft]}, #{two_complement_resp(addr_or_val)}"
    when 0b00001
      "bc1t #{float_register[instruction.ft]}, #{two_complement_resp(addr_or_val)}"
    end
  end

  def fr_to_print(instruction, funct: '', fmt: '', ft: '', fs: '', fd: '')
    case fmt
    when 0b00000
      rt = register[instruction.ft]

      "mfc1 #{rt}, #{fs}"
    when 0b10000
      case funct
      when 0b000000
        "add.s #{fd}, #{fs}, #{ft}"
      when 0b000001
        "sub.s #{fd}, #{fs}, #{ft}"
      when 0b000010
        "mul.s #{fd}, #{fs}, #{ft}"
      when 0b000110
        "mov.s #{fd}, #{fs}"
      when 0b111100
        "c.lt.s #{fd}, #{ft}"
      end
    when 0b10001
      case funct
      when 0b000000
        "add.d #{fd}, #{fs}, #{ft}"
      when 0b000010
        "mul.d #{fd}, #{fs}, #{ft}"
      when 0b000011
        "div.d #{fd}, #{fs}, #{ft}"
      when 0b000110
        "mov.d #{fd}, #{fs}"
      when 0b100000
        "cvt.s.d #{fd}, #{fs}"
      end
    when 0b10100
      "cvt.d.w #{fd}, #{fs}"
    when 0b00100
      rt = register[instruction.ft]

      "mtc1 #{rt}, #{fs}"
    end
  end

  def opcode_to_print(instruction, op:, rs: '', rt: '', addr_or_val: '')
    case op
    when 0b000001
      "bgez #{rs}, #{addr_or_val}"
    when 0b000010
      "j #{addr_or_val}"
    when 0b000011
      "jal #{addr_or_val}"
    when 0b000100
      "beq #{rs}, #{rt}, #{two_complement_resp(addr_or_val)}"
    when 0b000101
      "bne #{rs}, #{rt}, #{two_complement_resp(addr_or_val)}"
    when 0b000110
      "blez #{rs}, #{two_complement_resp(addr_or_val)}"
    when 0b001000
      "addi #{rt}, #{rs}, #{two_complement_resp(addr_or_val)}"
    when 0b001010
      "slti #{rt}, #{rs}, #{addr_or_val}"
    when 0b001100
      "andi #{rt}, #{rs}, #{addr_or_val}"
    when 0b001001
      "addiu #{rt}, #{rs}, #{two_complement_resp(addr_or_val)}"
    when 0b001101
      "ori #{rt}, #{rs}, #{addr_or_val}"
    when 0b001111
      "lui #{rt}, #{two_complement_resp(addr_or_val)}"
    when 0b100000
      "lb #{rt}, #{two_complement_resp(addr_or_val)}(#{rs})"
    when 0b100011
      "lw #{rt}, #{two_complement_resp(addr_or_val)}(#{rs})"
    when 0b101011
      "sw #{rt}, #{two_complement_resp(addr_or_val)}(#{rs})"
    when 0b110001
      ft = float_register[instruction.rt]

      "lwc1 #{ft}, #{two_complement_resp(addr_or_val)}(#{rs})"
    when 0b110101
      ft = float_register[instruction.rt]

      "ldc1 #{ft}, #{two_complement_resp(addr_or_val)}(#{rs})"
    when 0b111001
      ft = float_register[instruction.rt]

      "swc1 #{ft}, #{two_complement_resp(addr_or_val)}(#{rs})"
    end
  end

  def funct_to_print(instruction, funct:, rs: '', rt: '', rd: '', shamt: '')
    case funct
    when 0b000000
      if instruction.nop?
        'nop'
      else
        "sll #{rd}, #{rt}, #{shamt}"
      end
    when 0b000010
      "srl #{rd}, #{rt}, #{shamt}"
    when 0b000011
      "sra #{rd}, #{rt}, #{shamt}"
    when 0b001000
      "jr #{rs}"
    when 0b001001
      "jalr #{rd}, #{rs}"
    when 0b001100
      'syscall'
    when 0b001101
      'break'
    when 0b010000
      "mfhi #{rd}"
    when 0b010010
      "mflo #{rd}"
    when 0b011000
      "mult #{rs}, #{rt}"
    when 0b011010
      "div #{rs}, #{rt}"
    when 0b100000
      "add #{rd}, #{rs}, #{rt}"
    when 0b100001
      "addu #{rd}, #{rs}, #{rt}"
    when 0b100011
      "subu #{rd}, #{rs}, #{rt}"
    when 0b100100
      "and #{rd}, #{rs}, #{rt}"
    when 0b100101
      "or #{rd}, #{rs}, #{rt}"
    when 0b100110
      "xor #{rd}, #{rs}, #{rt}"
    when 0b101010
      "slt #{rd}, #{rs}, #{rt}"
    when 0b101011
      "sltu #{rd}, #{rs}, #{rt}"
    end
  end
end

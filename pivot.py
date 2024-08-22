import json

ops = json.load(open('../periodic-65c02/opcodes.json'))
"""
    "opcode": "00",
    "mnemonic": "BRK",
    "arg": null,
    "bytes": 2,
    "cycles": "7",
    "flags": "---I--"
"""

# opcode is aaabbbcc
# print layout c b x a

if False:
    for c in range(4):
        for b in range(8):
            print(f"{c} | {b} | ", end='')
            for a in range(8):
                op = ops[a << 5 | b << 2 | c]
    #            print(f"${op['opcode']} {op['bytes']} {op['mnemonic'].lower()} {op['arg'] or ''} | ", end='')
                print(f"{op['arg'] or op['bytes']} | ", end='')
    #            print(f"{op['mnemonic'][:3]} | ", end='')
            print()

    distinct = sorted({op['mnemonic'][:3] for op in ops})
    # print("\n".join(distinct))


    for hi in range(16):
        for lo in range(16):
            op = ops[hi*16+lo]
            print(op['mnemonic'], end=' ')
        print()

    from collections import Counter
    c = Counter([s[:2] for s in distinct])
    print(f"first pair {len(c)} distinct:", ', '.join(f"{k}: {n}" for k,n in c.items()))
    c = Counter([s[-2:] for s in distinct])
    print(f"last pair {len(c)} distinct:", ', '.join(f"{k}: {n}" for k,n in c.items()))
    for k in range(3):
        c = Counter([s[k] for s in distinct])
        print(f"char {k} {len(c)} distinct:", ', '.join(f"{k}: {n}" for k,n in c.items()))

for i,op in enumerate(ops):
    n = op['bytes']
    data = "00 " * (n-1) + "   " * (4-n)
    arg = op['arg']
    tgt = f"${i+n+4096:04x}"
    if arg in [None, 'A']:
        arg = ''
    else:
        arg = arg.replace("a16", "$0000").replace("a8","$00").replace("d8","$00").replace("r8",tgt)
    opcode = op['opcode']
    mnem = op['mnemonic']
    mnem = dict(DEA='DEC', INA='INC').get(mnem, mnem)
    s = f"10{opcode}   {opcode} {data}{mnem} {arg}".upper().strip()
    print(s)
    if (i+1)%16 == 0: print()

print()

for i in range(256):
    if not i%8: print()
    k = int(bin(i+256)[:2:-1], 2)
    op = ops[k]
    # print(f"{k:02x} ", end=' ')
    print(op['mnemonic'], end=' ')
#        print(op['opcode'], end=' ')
print()
"""End-to-end ELF regressions; no m68k toolchain or third-party modules needed."""
import pathlib
import struct
import subprocess
import sys
import tempfile
import unittest

BINARY = pathlib.Path(sys.argv.pop(1)).resolve() if len(sys.argv) > 1 else pathlib.Path('jlinker.native').resolve()


def elf(sections, symbols=(), relocations=(), rela=True):
    """Sections: (name, bytes/size for BSS, flags, alignment).

    Symbols: (name, section name or special index, value, size, binding).
    Relocations: (target section, offset, symbol index, type, addend).
    Symbol indices start at one, after the mandatory null symbol.
    """
    entries = [dict(name='', body=b'', typ=0, flags=0, align=0)]
    for name, body, flags, alignment in sections:
        entries.append(dict(name=name, body=b'' if isinstance(body, int) else body,
                            size=body if isinstance(body, int) else len(body),
                            typ=8 if isinstance(body, int) else 1, flags=flags, align=alignment))
    indices = {entry['name']: i for i, entry in enumerate(entries)}
    strings = bytearray(b'\0')
    table = bytearray(16)
    for name, section, value, size, binding in symbols:
        index = len(strings)
        strings += name.encode() + b'\0'
        table += struct.pack('>IIIBBH', index, value, size, binding << 4, 0,
                             indices[section] if isinstance(section, str) else section)
    symtab_index = len(entries)
    entries.append(dict(name='.symtab', body=table, typ=2, link=symtab_index + 1,
                        info=1 + sum(s[4] == 0 for s in symbols), align=4, entsize=16))
    entries.append(dict(name='.strtab', body=strings, typ=3, align=1))
    groups = {}
    for target, offset, symbol, typ, addend in relocations:
        if rela:
            record = struct.pack('>IIi', offset, (symbol << 8) | typ, addend)
        else:
            entry = entries[indices[target]]
            body = bytearray(entry['body'])
            struct.pack_into('>I', body, offset, addend & 0xffffffff)
            entry['body'] = body
            record = struct.pack('>II', offset, (symbol << 8) | typ)
        groups.setdefault(target, bytearray()).extend(record)
    for target, body in groups.items():
        entries.append(dict(name=('.rela' if rela else '.rel') + target, body=body,
                            typ=4 if rela else 9, link=symtab_index, info=indices[target],
                            align=4, entsize=12 if rela else 8))
    shstr_index = len(entries)
    entries.append(dict(name='.shstrtab', body=b'', typ=3, align=1))
    names = bytearray(b'\0')
    for entry in entries[1:]:
        entry['name_offset'] = len(names)
        names += entry['name'].encode() + b'\0'
    entries[-1]['body'] = names
    content = bytearray(52)
    headers = []
    for entry in entries:
        if entry['typ'] == 0:
            headers.append(bytes(40))
            continue
        alignment = max(1, entry['align'])
        content += b'\0' * (-len(content) % alignment)
        offset = len(content)
        content += entry['body']
        headers.append(struct.pack('>10I', entry.get('name_offset', 0), entry['typ'],
                                   entry.get('flags', 0), 0, offset,
                                   entry.get('size', len(entry['body'])), entry.get('link', 0),
                                   entry.get('info', 0), entry['align'], entry.get('entsize', 0)))
    content += b'\0' * (-len(content) % 4)
    shoff = len(content)
    content += b''.join(headers)
    content[:52] = struct.pack('>16sHHIIIIIHHHHHH', b'\x7fELF\x01\x02\x01' + b'\0' * 9,
                              1, 4, 1, 0, 0, shoff, 0, 52, 0, 0, 40, len(entries), shstr_index)
    return content


def words(data):
    return struct.unpack('>' + 'I' * (len(data) // 4), data)


class ElfTests(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp.cleanup)
        self.root = pathlib.Path(self.temp.name)
        self.serial = 0

    def file(self, content, suffix='.o'):
        self.serial += 1
        path = self.root / (str(self.serial) + suffix)
        path.write_bytes(content)
        return path

    def run_link(self, inputs, options=(), failure=None, output=None):
        output = output or self.root / 'output.bin'
        result = subprocess.run([str(BINARY), *options, '-o', str(output), *map(str, inputs)],
                                capture_output=True, text=True)
        if failure is not None:
            self.assertNotEqual(result.returncode, 0)
            self.assertIn(failure, result.stdout + result.stderr)
            return output
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        return output.read_bytes()

    def absolute(self, inputs, layout=('1000', 'x', 'x'), options=()):
        return self.run_link(inputs, ('-a', *layout, *options))

    def basic(self, rela=True, debug=False, alignment=1):
        sections = [('.text', bytes(8), 6, alignment), ('.data', b'D' * 8, 3, alignment),
                    ('.bss', 8, 3, alignment)]
        relocs = [('.text', 0, 1, 1, 0), ('.text', 4, 2, 1, 0)]
        if debug:
            sections.append(('.debug_info', bytes(4), 0, 1))
            # Unsupported relocation is harmless in a discarded debug section.
            relocs.append(('.debug_info', 0, 1, 4, 0))
        return self.file(elf(sections, [('d', '.data', 0, 0, 1), ('b', '.bss', 0, 0, 1)], relocs, rela))

    def test_data_bss_addresses_rel_and_rela(self):
        for rela in (False, True):
            with self.subTest(rela=rela):
                output = self.absolute([self.basic(rela)])
                self.assertEqual(words(output[36:44]), (0x1008, 0x1010))

    def test_external_symbols_and_noncontiguous_layout(self):
        definitions = self.basic()
        consumer = self.file(elf([('.text', bytes(8), 6, 1)],
                                 [('d', 0, 0, 0, 1), ('b', 0, 0, 0, 1)],
                                 [('.text', 0, 1, 1, 3), ('.text', 4, 2, 1, -4)]))
        output = self.absolute([consumer, definitions], ('2000', '4000', '8000'))
        self.assertEqual(words(output[36:52]), (0x4003, 0x7ffc, 0x4000, 0x8000))

    def test_subsections_and_data_relocation_locations(self):
        source = self.file(elf(
            [('.text', b'T' * 8, 6, 1), ('.data.first', b'D' * 4, 3, 1),
             ('.data.second', bytes(8), 3, 8), ('.bss.first', 3, 3, 1),
             ('.bss.second', 4, 3, 8)],
            [('d', '.data.second', 4, 0, 0), ('b', '.bss.second', 1, 0, 0)],
            [('.data.second', 0, 1, 1, 0), ('.data.second', 4, 2, 1, 0)]))
        output = self.absolute([source])
        self.assertEqual(output[44:52], b'D' * 4 + bytes(4))
        self.assertEqual(words(output[52:60]), (0x1014, 0x1021))

    def test_debug_relocations_are_discarded(self):
        self.assertEqual(self.absolute([self.basic(debug=True)]), self.absolute([self.basic()]))

    def test_invalid_debug_relocation_target_is_rejected(self):
        source = self.basic(debug=True)
        content = bytearray(source.read_bytes())
        shoff = struct.unpack_from('>I', content, 32)[0]
        count = struct.unpack_from('>H', content, 48)[0]
        for i in range(count):
            offset = shoff + i * 40
            if struct.unpack_from('>I', content, offset + 4)[0] == 4:
                struct.pack_into('>I', content, offset + 28, count)
        source.write_bytes(content)
        self.run_link([source], ('-a', '1000', 'x', 'x'), 'invalid ELF relocation target')

    def test_allocated_unsupported_relocation_still_fails(self):
        source = self.file(elf([('.text', bytes(4), 6, 1)], [('x', '.text', 0, 0, 1)],
                               [('.text', 0, 1, 4, 0)]))
        self.run_link([source], ('-a', '1000', 'x', 'x'), 'unsupported m68k ELF relocation')

    def test_contiguous_alignment_and_padding(self):
        source = self.file(elf([('.text', b'T' * 8, 6, 8), ('.data', b'D' * 8, 3, 32),
                               ('.bss', 8, 3, 64)]))
        output = self.absolute([source])
        self.assertEqual(words(output[2:14]), (32, 32, 8))
        self.assertEqual(words(output[28:36]), (0x1020, 0x1040))
        self.assertEqual(output[36:], b'T' * 8 + bytes(24) + b'D' * 8 + bytes(24))
        raw = self.absolute([source], options=('-n',))
        self.assertEqual(raw, output[36:])

    def test_object_alignment_after_unaligned_predecessor(self):
        first = self.file(elf([('.text', bytes(4), 6, 1)]))
        second = self.file(elf([('.text', bytes(4), 6, 32)], [('x', '.text', 0, 0, 1)],
                               [('.text', 0, 1, 1, 0)]))
        output = self.absolute([first, second], options=('-rw',))
        self.assertEqual(words(output[2:6]), (36,))
        self.assertEqual(words(output[68:72]), (0x1020,))

    def test_contiguous_alignment_accounts_for_base_address(self):
        source = self.file(elf([('.text', b'TT', 6, 2), ('.data', b'DD', 3, 32)]))
        output = self.absolute([source], ('1002', 'x', 'x'))
        self.assertEqual(words(output[2:6]), (30,))
        self.assertEqual(words(output[28:32]), (0x1020,))
        self.assertEqual(output[36:66], b'TT' + bytes(28))

    def test_explicit_misaligned_addresses_are_rejected(self):
        for segment in range(3):
            with self.subTest(segment=segment):
                layout = ['1000', '2000', '3000']
                layout[segment] = '1002'
                self.run_link([self.basic(alignment=8)], ('-a', *layout),
                              'does not satisfy ELF alignment 8')

    def test_common_symbol_alignment_and_size(self):
        first = self.file(elf([('.text', bytes(4), 6, 1), ('.bss', 3, 3, 1)],
                              [('c', 0xfff2, 8, 4, 1)], [('.text', 0, 1, 1, 0)]))
        second = self.file(elf([], [('c', 0xfff2, 64, 16, 1)]))
        output = self.absolute([first, second])
        self.assertEqual(words(output[32:36]), (0x1040,))
        self.assertEqual(words(output[36:40]), (0x1080,))
        self.assertEqual(words(output[10:14]), (80,))

    def test_partial_round_trip_without_alignment_constraints(self):
        source = self.basic()
        direct = self.absolute([source])
        partial = self.file(self.run_link([source], ('-p',)))
        self.assertEqual(self.absolute([partial]), direct)

    def test_partial_rejects_alignment_loss_before_opening_output(self):
        output = self.file(b'previous output')
        self.run_link([self.basic(alignment=8)], ('-p',),
                      'cannot preserve ELF alignment', output)
        self.assertEqual(output.read_bytes(), b'previous output')

    def test_partial_common_alignment_loss_is_rejected(self):
        source = self.file(elf([], [('c', 0xfff2, 32, 4, 1)]))
        for option in ('-p', '-q'):
            with self.subTest(option=option):
                self.run_link([source], (option,), 'cannot preserve ELF alignment')

    def test_fully_absolute_partial_link_has_no_remaining_constraints(self):
        source = self.basic(alignment=8)
        output = self.run_link([source], ('-q', '-a', '1000', 'x', 'x'))
        self.assertEqual(words(output[32:40]), (0x1008, 0x1010))

    def test_elf_archive_member_is_selected(self):
        member = self.basic(alignment=8).read_bytes()
        header = ('%-16s%-12s%-6s%-6s%-8s%-10d`\n' %
                  ('defs.o/', '0', '0', '0', '644', len(member))).encode()
        archive = self.file(b'!<arch>\n' + header + member + b'\n' * (len(member) % 2), '.a')
        consumer = self.file(elf([('.text', bytes(4), 6, 1)], [('d', 0, 0, 0, 1)],
                                 [('.text', 0, 1, 1, 0)]))
        self.assertEqual(words(self.absolute([consumer, archive])[36:40]), (0x1010,))

    def test_elf_and_aout_objects_can_be_mixed(self):
        payload = self.file(b'AAAA', '.bin')
        legacy = self.file(self.run_link([], ('-p', '-i', str(payload), 'blob')))
        output = self.absolute([legacy, self.basic()])
        self.assertEqual(words(output[36:44]), (0x1010, 0x1018))
        self.assertEqual(output[44:52], b'AAAA' + bytes(4))

    def test_partial_preserves_legacy_aout_linking(self):
        payload = self.file(b'DATA', '.bin')
        partial = self.file(self.run_link([], ('-p', '-i', str(payload), 'blob')))
        self.assertEqual(self.absolute([partial])[36:], b'DATA' + bytes(4))


if __name__ == '__main__':
    unittest.main()

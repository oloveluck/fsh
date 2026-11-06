import re,glob,json,sys,os
src=sys.argv[1]; out=sys.argv[2]
cases=[]; skipped=0
def unescape_tpl(t):
    # template literal: handle \` and \\ and ${...} (skip cases with interpolation)
    if '${' in t: return None
    return t.replace('\\`','`').replace('\\\\','\\')
def left_align(t):
    lines=t.split('\n')
    while lines and lines[0].strip()=='': lines.pop(0)
    while lines and lines[-1].strip()=='': lines.pop()
    ind=min((len(l)-len(l.lstrip()) for l in lines if l.strip()), default=0)
    return '\n'.join(l[ind:] for l in lines)
for f in sorted(glob.glob(os.path.join(src,'FSHImporter*.test.ts')))+[os.path.join(src,'FSHErrorListener.test.ts')]:
    s=open(f).read()
    # split into it(...) blocks
    for m in re.finditer(r"\n([ \t]*)it\((['\"`])(.*?)\2,.*?\n", s):
        start=m.end(); indent=m.group(1); name=m.group(3)
        # block ends at next line that is exactly indent + "});"
        end=s.find('\n'+indent+'});', start)
        if end<0: continue
        body=s[start:end]
        # input: const input = leftAlign(`...`) or `...`
        im=re.search(r"const input\s*=\s*(leftAlign\()?`(.*?)`\)?;", body, re.S)
        if not im:
            skipped+=1; continue
        txt=unescape_tpl(im.group(2))
        if txt is None: skipped+=1; continue
        if im.group(1): txt=left_align(txt)
        if 'importSingleText(input' not in body: skipped+=1; continue
        fm=re.search(r"importSingleText\(input,\s*'([^']+)'\)", body)
        fname=fm.group(1) if fm else 'Test.fsh'
        expects=[]
        for em in re.finditer(r"getLastMessage\('(error|warn|info)'\)\)\.toMatch\(\s*(/(.*?)/([a-z]*)|'((?:[^'\\]|\\.)*)')\s*\)", body, re.S):
            lvl=em.group(1); rx=em.group(3) if em.group(3) is not None else re.escape(em.group(5))
            expects.append({'level':lvl,'regex':rx})
        none_expected=bool(re.search(r"getAllMessages\('(error|warn)'\)\)\.toHaveLength\(0\)|getAllLogs\('(error|warn)'\)\)\.toHaveLength\(0\)|getAllMessages\(\)\)\.toHaveLength\(0\)", body))
        cases.append({'file':os.path.basename(f),'name':name,'fsh_file':fname,'input':txt,'expects':expects,'no_messages':none_expected})
json.dump(cases, open(out,'w'), indent=1)
print(len(cases),'cases;',skipped,'skipped;', sum(1 for c in cases if c['expects']),'with expected messages;', sum(1 for c in cases if c['no_messages']),'asserting none')

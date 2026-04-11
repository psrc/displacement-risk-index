# displacement-risk-index
This code calculates individual displacement risk indicators and the consolidated risk index. Learn more at https://www.psrc.org/our-work/displacement-risk-mapping

## Python setup (for the helper scripts)

Some folders include Python utilities (e.g., distance calculations). The recommended convention is a repo-root virtual environment named `.venv/` and a tracked `requirements.txt`.

### Windows (PowerShell)

```powershell
py -m venv .venv
.\.venv\Scripts\Activate.ps1
python -m pip install --upgrade pip
pip install -r requirements.txt
```

### macOS/Linux (bash/zsh)

```bash
python3 -m venv .venv
source .venv/bin/activate
python -m pip install --upgrade pip
pip install -r requirements.txt
```

To leave the environment: `deactivate`.

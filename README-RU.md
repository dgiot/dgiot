# dgaiot — Промышленный IoT-движок агрегации

Высокопроизводительная IoT-платформа на Erlang/OTP.

## Ключевые компоненты

- Теневое устройство: gen_statem 1:1
- Онтология: Site > Gateway > Device > Point
- Конечный автомат: init -> auth -> online -> {normal, alarm, offline}

## Архитектура

iotStudio(край) --MQTT--> dgaiot(агрегация) <--HTTP-- iotStudio(приложение)

## Модули

dgiot/ ядро | dgiot_ontology/ онтология | dgiot_parse/ Parse | dgiot_task/ тени | dgiot_device/ устройства | dgiot_bridge/ мост | dgiot_dlink/ канал | dgiot_api/ API | dgiot_http/ HTTP | dgiot_tdengine/ временные ряды

## Сборка

export PATH=/usr/local/erlang_24.3/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games:/usr/local/games:/usr/lib/wsl/lib:/mnt/c/Users/Administrator/bin:/mnt/c/Program Files/Git/mingw64/bin:/mnt/c/Program Files/Git/usr/local/bin:/mnt/c/Program Files/Git/usr/bin:/mnt/c/Program Files/Git/usr/bin:/mnt/c/Program Files/Git/mingw64/bin:/mnt/c/Program Files/Git/usr/bin:/mnt/c/Users/Administrator/bin:/mnt/c/Python314/Scripts:/mnt/c/Python314:/mnt/d/Program Files/ffmpeg-7.1.1-essentials_build/bin:/mnt/d/miniconda3/condabin:/mnt/c/WINDOWS/system32:/mnt/c/WINDOWS:/mnt/c/WINDOWS/System32/Wbem:/mnt/c/WINDOWS/System32/WindowsPowerShell/v1.0:/mnt/c/WINDOWS/System32/OpenSSH:/mnt/c/Program Files/TortoiseSVN/bin:/mnt/c/Program Files/PuTTY:/mnt/c/Program Files (x86)/NVIDIA Corporation/PhysX/Common:/mnt/c/ProgramData/chocolatey/bin:/mnt/c/Program Files/dotnet:/mnt/c/Program Files/NVIDIA Corporation/NVIDIA app/NvDLISR:/mnt/d/miniconda3/condabin:/mnt/c/Program Files/Git/cmd:/mnt/c/Users/Administrator/.cargo/bin:/mnt/c/Users/Administrator/AppData/Roaming/local/bin:/mnt/c/Users/Administrator/AppData/Local/Microsoft/WindowsApps:/mnt/c/Program Files/JetBrains/IntelliJ IDEA Community Edition 2023.2/bin:/mnt/c/Users/Administrator/AppData/Local/Programs/Ollama:/mnt/d/msys64/data/bin/erlang/bin:/mnt/c/Users/Administrator/.lmstudio/bin:/mnt/d/miniconda3/condabin:/mnt/c/TDengine:/mnt/c/Users/Administrator/.local/bin:/mnt/d/Program Files/nodejs:/mnt/c/Program Files/MiKTeX/miktex/bin/x64:/mnt/c/Users/Administrator/AppData/Local/Programs/Python/Python311/Scripts:/mnt/c/Users/Administrator/AppData/Local/Programs/Python/Python311:/mnt/c/Users/Administrator/.local/bin:/mnt/c/Users/Administrator/AppData/Local/Microsoft/WindowsApps:/mnt/c/Program Files/JetBrains/IntelliJ IDEA Community Edition 2023.2/bin:/mnt/c/Users/Administrator/AppData/Local/Programs/Ollama:/mnt/d/msys64/data/bin/erlang/bin:/mnt/c/Users/Administrator/.lmstudio/bin:/mnt/d/miniconda3/condabin:/mnt/c/Users/Administrator/AppData/Local/Pandoc:/mnt/c/Users/Administrator/.local/bin:/mnt/d/Program Files/CodeBuddy CN/bin:/mnt/d/Program Files/Microsoft VS Code/bin:/mnt/c/Users/Administrator/AppData/Roaming/npm:/mnt/c/Users/Administrator/AppData/Local/Python/bin:/mnt/c/Program Files/Git/usr/bin/vendor_perl:/mnt/c/Program Files/Git/usr/bin/core_perl && make

## Лицензия

Apache 2.0

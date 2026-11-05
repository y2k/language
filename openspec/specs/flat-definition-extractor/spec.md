# flat-definition-extractor Specification

## Purpose

Определяет распаковку текстового FEX bundle в отдельные файлы RHS для последующей доставки и загрузки исполнителем. Формат bundle и canonical keys задаёт `compiler-targets`.

## Requirements

### Requirement: CLI SHALL предоставлять отдельную распаковку FEX bundle

CLI SHALL принимать `--unpack-fex --output-dir PATH`, читать bundle из stdin и последовательно материализовать файлы в указанном каталоге, создавая каталог при необходимости. Unpacker SHALL декодировать payload без разбора или исполнения FEX и сохранять bytes точно. После успешной обработки каждой пары stdout SHALL печатать её key с LF; диагностика SHALL идти в stderr. Output directory SHALL быть обязательным для распаковки.

#### Scenario: Материализация файла
- **WHEN** unpacker получает пару `app.logic.answer` и `aW50IDQyCg==`
- **THEN** файл `app.logic.answer` в output directory содержит `int 42` с завершающим LF
- **AND** stdout содержит `app.logic.answer` с LF

#### Scenario: Распаковка без каталога
- **WHEN** CLI получает `--unpack-fex` без `--output-dir`
- **THEN** возвращается ошибка без записи code-файлов

### Requirement: Unpacker SHALL соблюдать границы строк контейнера

Unpacker SHALL принимать LF, CRLF и последний payload без завершающего LF, не пропуская пустые строки. EOF между парами SHALL завершать ввод; EOF после имени без payload и некорректный Base64 SHALL приводить к ошибке. Завершённость всего пакета при обрыве между парами не гарантируется. Переводы строк декодированного RHS SHALL NOT нормализоваться или дополняться.

#### Scenario: Варианты перевода строки
- **WHEN** одна пара передана с LF, CRLF или без финального LF после payload
- **THEN** создаются одинаковые bytes файла RHS
- **AND** отсутствие LF внутри декодированного RHS сохраняется

#### Scenario: Незавершённая пара
- **WHEN** после строки имени наступает EOF без строки payload
- **THEN** unpacker сообщает ошибку

#### Scenario: Некорректный payload
- **WHEN** строка payload содержит некорректный Base64
- **THEN** unpacker сообщает ошибку вместо записи этого payload

### Requirement: Unpacker SHALL обновлять только изменившиеся файлы

Unpacker SHALL сохранять существующий файл и его mtime при совпадении bytes, иначе перезаписывать файл. Повторный key SHALL обрабатываться тем же правилом: последнее вхождение побеждает. stdout SHALL включать неизменившиеся и повторные keys в порядке обработки. Соседние и устаревшие файлы SHALL NOT удаляться. Предварительная проверка всего пакета, атомарная запись и rollback не гарантируются; ошибка может оставить частично обновлённый каталог.

#### Scenario: Неизменившееся определение
- **WHEN** файл уже содержит bytes распаковываемого RHS
- **THEN** его содержимое и mtime остаются прежними
- **AND** его key всё равно печатается в stdout

#### Scenario: Повторный key
- **WHEN** bundle содержит два вхождения одного key с разными RHS
- **THEN** файл содержит RHS последнего вхождения
- **AND** stdout перечисляет key дважды

#### Scenario: Соседние файлы
- **WHEN** bundle обновляет одно определение в каталоге с другими файлами
- **THEN** остальные файлы остаются нетронутыми

### Requirement: Unpacker SHALL использовать безопасные canonical filenames

Unpacker SHALL принимать только безопасные canonical filenames по контракту `compiler-targets`, не допускающие выхода из output directory. Key SHALL использоваться буквально без снятия escapes, повторного кодирования или case folding.

#### Scenario: Небезопасное имя во входном bundle
- **WHEN** unpacker получает имя с выходом за output directory, например `../outside`
- **THEN** он сообщает ошибку и не записывает файл по этому имени

#### Scenario: Различие регистра и буквального маркера
- **WHEN** bundle содержит keys `app.foo`, `app.!foo`, `app.!f!o!o` и `app.%21foo`
- **THEN** распаковка создаёт четыре отдельных файла даже на регистронезависимой FS
- **AND** filenames и stdout сохраняют keys буквально

# Telebot

Фильтр по задачам из редмайна. Список задач обновляется раз в 3 минуты.
 Список сохраняется в кеш, можно не бояться делать много запросов.
 Ограничение на вывод задач 10 штук. Для более подробного вывода, используйте фильтры
 
 Список возможностей:
 * Свободный поиск по задачам, команды /s, /ss.
   /s - Поиск задач, в качестве предиката используется "ИЛИ".
   Например: /s 1.5 Ожидает деплоя - найдет все задачи из версии 1.5 или содержащие слова: Ожидает, деплоя.
   /ss - аналог /s но предикат "И".
   Например: /ss 1.5 Ожидает деплоя - найдет все задачи из версии 1.5 ожидающие деплоя
 
   Фильтровать можно по описанию, версии, статусу, автору, назначенному, id, трекеру, приоритету
 
 * Заранее подготовленные фильтры
   Все фильтры поддерживают семантику ss
 
   * /need_review - Список задач, ожидающих ревью
   * /on_review - Список задач на ревью
   * /new_dev - Новые задачи, назначенные на наш отдел
   * /ready_to_deploy - Задачи, готовые к деплою
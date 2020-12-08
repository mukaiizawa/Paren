; database module.

(function column->ddl (column)
  (let ((column-name :key primarykey? foreignkey required? type size default-value) column)
    (string  "	" column-name " " (mem->str type) (if size (string "(" size ")"))
             (if default-value (string " default " default-value))
             (if (|| required? primarykey?) " not null"))))

(function table->ddl (table)
  (let ((table-name :rest columns) table)
    (string "create table " table-name " (\n"
            (join (map column->ddl columns) ",\n")
            ");\n\n")))

(function table-key-columns (table key)
  (select (f (x) (nilable-assoc (cdr x) key))
          (cdr table)))

(function x.y->pairs (x.y)
  (let (x.y (mem->str x.y) i (memmem x.y "."))
    (list (submem x.y 0 i)
          (submem x.y (++ i)))))

(function table->constraints (table)
  (let (primarykeys (table-key-columns table :primarykey?)
                    foreignkeys (table-key-columns table :foreignkey))
    (join
      (append
        (if primarykeys
            (list
              (string "alter table " (car table) " add primary key (" (join (map car primarykeys) ", ") ");\n")))
        (map (f (x)
               (let (fk (assoc (cdr x) :foreignkey) (fk-table fk-column) (x.y->pairs fk))
                 (string "alter table " (car table) " add constraints fk_" (car x) " foreign key(" (car x) ") references " fk-table "(" fk-column ");\n")))
             foreignkeys)))))

(function tables->ddl (tables)
  ; Returns a list representing a table in the table definition language.
  (join (except nil?
                (append
                  (map table->ddl tables)
                  (map table->constraints tables)))))

(function select-query (tables columns :opt clauses)
  (string "select " (join (->list columns) ", ")
          " from " (join (->list tables) ", ")))

(function! main (args)
  (let (tables '((users
                   (id :primarykey? true :type "number(10)")
                   (name :required? true :type "varchar(10)"))
                 (products
                   (id :primarykey? true :type "number(10)")
                   (name :required? true :type "varchar(10)")
                   (price :required? true :type "number(10)"))
                 (reviews
                   (user_id :primarykey? true :foreignkey users.id :type "number(10)")
                   (product_id :primarykey? true :foreignkey products.id :type "number(10)")
                   (text :required? true :type "varchar(1000)"))))
    (write-line (tables->ddl tables))
    (write-line (select-query :users '(:id :name)))))

// garbage collector


void gc_init(void)
{
  int i;
  object o;
  new_top=om_table.size;
  last_old_space_size=om_table.size;
  last_new_space_size=0;
  xarray_init(&refnew_table);
  xarray_init(&mark_stack);
  for(i=0;i<om_table.size;i++) {
    o=om_table.elt[i];
    set_generation(o,OLD);
    set_alive(o,FALSE);
    set_refnew(o,FALSE);
  }
}

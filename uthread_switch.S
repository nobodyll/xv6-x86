	.text

/* Switch from current_thread to next_thread. Make next_thread
 * the current_thread, and set next_thread to 0.
 * Use eax as a temporary register; it is caller saved.
 */
	.globl thread_switch
thread_switch:
	/* YOUR CODE HERE */
	// Save the current thread's register into his stack
	pushal	

	// Save the current esp resgister's value to struct current_thread->sp
	movl current_thread, %eax
	movl %esp, (%eax)

	// Switch stack to next_thread
	movl next_thread, %ebx
	movl (%ebx), %esp
	
	// set the current thread = next thread
	leal current_thread, %ecx
	leal next_thread, %edx
	movl %ebx, (%ecx)

	// set the next_thread = 0;
	movl $0, (%edx)

	// Restore the next_thread's register
	popal

	ret				/* pop return address from stack */

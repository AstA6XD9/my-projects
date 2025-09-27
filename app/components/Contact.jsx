"use client"
import React, { useState } from "react";
import { assets } from "@/assets/assets";
import Image from "next/image";
import { motion } from "framer-motion";

const Contact = ({ isDarkMode }) => {
    const [result, setResult] = useState("");
    const onSubmit = async (event) => {
        event.preventDefault();
        setResult("Sending....");
        const formData = new FormData(event.target);
    
        formData.append("access_key", "95bf21ac-681a-4183-a718-a3dd1dc182e5");
    
        const response = await fetch("https://api.web3forms.com/submit", {
          method: "POST",
          body: formData
        });
    
        const data = await response.json();
    
        if (data.success) {
          setResult("Form Submitted Successfully");
          event.target.reset();
        } else {
          console.log("Error", data);
          setResult(data.message);
        }
      };
    return (
        <motion.div 
          initial={{ opacity: 0 }}
          whileInView={{ opacity: 1 }}
          transition={{ duration: 0.4 }}
          viewport={{ once: true }}
          id='contact' 
          className={`w-full px-[12%] py-10 scroll-mt-20 ${isDarkMode ? 'bg-gray-900 text-white' : 'bg-gray-50 text-black'}`}
        >
            <motion.div
              initial={{ y: 20, opacity: 0 }}
              whileInView={{ y: 0, opacity: 1 }}
              transition={{ duration: 0.4 }}
              viewport={{ once: true }}
            >
              <h4 className="text-center mb-2 text-lg font-Ovo">
                  Connect with me
              </h4>
              <h2 className="text-center text-5xl font-Ovo">  Get in touch</h2>
              <p className="text-center max-w-2xl mx-auto mt-5 mb-12 font-Ovo">
                  I'd love to hear from you! Whether you have a question, a project idea, or just want to say hi, please feel free to reach out.
              </p>
            </motion.div>
             
             <motion.form 
               initial={{ y: 20, opacity: 0 }}
               whileInView={{ y: 0, opacity: 1 }}
               transition={{ duration: 0.4, delay: 0.1 }}
               viewport={{ once: true }}
               onSubmit={onSubmit} 
               className="w-full max-w-2xl mx-auto flex flex-col items-center"
             >
                 <div className="w-full grid grid-cols-1 md:grid-cols-2 gap-6 mt-10 mb-8">
                     <input 
                         type="text" 
                         placeholder="Enter your name" 
                         required 
                         className={`flex-1 p-3 outline-none border-[0.5px] border-gray-400 rounded-md font-Ovo ${isDarkMode ? 'bg-gray-800 text-white placeholder-gray-300' : 'bg-white text-black placeholder-gray-500'}`}
                         name='name' 
                     /> 
                     <input 
                         type="email" 
                         placeholder="Enter your email" 
                         className={`flex-1 p-3 outline-none border-[0.5px] border-gray-400 rounded-md font-Ovo ${isDarkMode ? 'bg-gray-800 text-white placeholder-gray-300' : 'bg-white text-black placeholder-gray-500'}`}
                         required 
                         name='email' 
                     />
                 </div>
                 <textarea 
                     rows={6} 
                     placeholder="Enter your message" 
                     required 
                     className={`w-full p-4 outline-none border-[0.5px] border-gray-400 rounded-md mb-6 font-Ovo ${isDarkMode ? 'bg-gray-800 text-white placeholder-gray-300' : 'bg-white text-black placeholder-gray-500'}`}
                     name="message" 
                 />
                 <button 
                     className={`py-3 px-8 w-max flex items-center justify-between gap-2 rounded-full mx-auto duration-500 font-Ovo ${isDarkMode ? 'bg-black/80 text-white hover:bg-black' : 'bg-white text-black hover:bg-gray-100 border border-gray-300'}`}
                     type="submit"
                 >
                     Send <Image src={isDarkMode ? assets.right_arrow_white : assets.right_arrow} alt="Send" className="w-4" />
                 </button>
                 <p className="mt-4 text-center font-Ovo">{result}</p>
             </motion.form>
        </motion.div>
    );
};

export default Contact;
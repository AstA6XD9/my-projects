import React from "react";
import Image from "next/image";
import { motion } from "framer-motion";
import { assets } from "@/assets/assets";

const Footer = ({ isDarkMode }) => {
    return (
        <motion.div 
          initial={{ opacity: 0 }}
          whileInView={{ opacity: 1 }}
          transition={{ duration: 0.4 }}
          viewport={{ once: true }}
          className="mt-20"
        >
            <motion.div 
              initial={{ y: 20, opacity: 0 }}
              whileInView={{ y: 0, opacity: 1 }}
              transition={{ duration: 0.4 }}
              viewport={{ once: true }}
              className="text-center"
            >
                <motion.div
                  initial={{ opacity: 0 }}
                  whileInView={{ opacity: 1 }}
                  transition={{ duration: 0.4, delay: 0.1 }}
                  viewport={{ once: true }}
                >
                  <Image src={isDarkMode ? assets.logo_dark : assets.logo} alt="Logo" className="w-36 mx-auto mb-2" />
                </motion.div>
                <motion.div 
                  initial={{ y: 20, opacity: 0 }}
                  whileInView={{ y: 0, opacity: 1 }}
                  transition={{ duration: 0.4, delay: 0.2 }}
                  viewport={{ once: true }}
                  className="w-max flex items-center gap-2 mx-auto font-Ovo"
                >
                    <Image src={assets.mail_icon} alt="Email" className="w-6 h-6" />
                    elouardinimohamedamine@gmail.com
                </motion.div>
            </motion.div>
            
            <motion.div 
              initial={{ y: 20, opacity: 0 }}
              whileInView={{ y: 0, opacity: 1 }}
              transition={{ duration: 0.4, delay: 0.3 }}
              viewport={{ once: true }}
              className="text-center sm:flex items-center justify-between border-t border-gray-400 mx-[10%] mt-12 py-0"
            > 
              <p className="text-center font-Ovo">© 2025 M-Amine El Ouardini. All rights reserved.</p> 
            </motion.div>
            
            <motion.ul 
              initial={{ y: 20, opacity: 0 }}
              whileInView={{ y: 0, opacity: 1 }}
              transition={{ duration: 0.4, delay: 0.4 }}
              viewport={{ once: true }}
              className="flex items-center gap-10 justify-center mt-4 sm:mt-0"
            >
                 <motion.li
                   initial={{ opacity: 0 }}
                   whileInView={{ opacity: 1 }}
                   transition={{ duration: 0.3, delay: 0.5 }}
                   viewport={{ once: true }}
                 >
                   <a href="https://www.linkedin.com/in/mohamed-amine-el-ouardini-01a852284/" target="_blank" rel="noopener noreferrer" className="font-Ovo">LinkedIn</a>
                 </motion.li>
                 <motion.li
                   initial={{ opacity: 0 }}
                   whileInView={{ opacity: 1 }}
                   transition={{ duration: 0.3, delay: 0.6 }}
                   viewport={{ once: true }}
                 >
                   <a href="https://github.com/mohamedamineelouardini" target="_blank" rel="noopener noreferrer" className="font-Ovo">GitHub</a>
                 </motion.li>
             </motion.ul>
        </motion.div>
    );
};

export default Footer;
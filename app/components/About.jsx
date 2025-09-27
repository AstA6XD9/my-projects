import React from "react"
import Image from "next/image"
import { motion } from "framer-motion"
import { assets, infoList } from "../../assets/assets"
import { toolsData } from "../../assets/assets"

export default function About({ isDarkMode }) {
    return (
        <motion.div 
          initial={{ opacity: 0 }}
          whileInView={{ opacity: 1 }}
          transition={{ duration: 0.4 }}
          viewport={{ once: true }}
          id='about' 
          className={`w-full px-[12%] py-10 scroll-mt-20 ${isDarkMode ? 'bg-gray-900 text-white' : 'bg-gray-50 text-black'}`}
        >
            <motion.div
              initial={{ y: 20, opacity: 0 }}
              whileInView={{ y: 0, opacity: 1 }}
              transition={{ duration: 0.4 }}
              viewport={{ once: true }}
            >
              <h4 className="text-center mb-2 text-lg font-Ovo">Introduction</h4>
              <h2 className="text-center text-5xl font-Ovo">About me</h2>
            </motion.div>
            
            <div className="flex w-full flex-col lg:flex-row items-start gap-20 my-20">
                <motion.div 
                  initial={{ opacity: 0 }}
                  whileInView={{ opacity: 1 }}
                  transition={{ duration: 0.4, delay: 0.1 }}
                  viewport={{ once: true }}
                  className="w-full lg:w-1/2 rounded-3xl -mt-8"
                >
                    <Image src={assets.user_image} alt="User" className="w-full h-[28rem] sm:h-[32rem] lg:h-[34rem] object-cover rounded-3xl" />
                </motion.div>
                
                <motion.div 
                  initial={{ opacity: 0 }}
                  whileInView={{ opacity: 1 }}
                  transition={{ duration: 0.4, delay: 0.2 }}
                  viewport={{ once: true }}
                  className="w-full lg:w-1/2"
                >
                    <motion.p 
                      initial={{ y: 20, opacity: 0 }}
                      whileInView={{ y: 0, opacity: 1 }}
                      transition={{ duration: 0.4, delay: 0.3 }}
                      viewport={{ once: true }}
                      className="mb-10 max-w-2xl font-Ovo"
                    >
                    I completed two years of preparatory classes in Mathematics and Physics (MPSI/MP), where I built a solid foundation in mathematics and physics. After passing the national entrance exams, I was admitted to ENSEEIHT in Digital Sciences. During my first generalist year, I acquired broad knowledge in areas such as imperative programming, object-oriented technologies, scientific computing, and data analysis, including an introduction to deep learning. I then completed an internship at a startup specialized in creating Moroccan traditional salons using 3D models, where I worked on shadow removal and gained practical experience with computer vision, OpenCV, and deep learning. Through these experiences, I developed a strong professional interest in image and multimedia processing, which I chose to specialize in during my second year at ENSEEIHT.
                    </motion.p>
                    
                    <motion.ul 
                      initial={{ y: 20, opacity: 0 }}
                      whileInView={{ y: 0, opacity: 1 }}
                      transition={{ duration: 0.4, delay: 0.4 }}
                      viewport={{ once: true }}
                      className="grid grid-cols-1 sm:grid-cols-2 gap-6"
                    >
                        {infoList.map(({ icon,iconDark, title, description }, index) => (
                            <motion.li 
                              initial={{ opacity: 0 }}
                              whileInView={{ opacity: 1 }}
                              transition={{ duration: 0.3, delay: 0.5 + index * 0.1 }}
                              viewport={{ once: true }}
                              tabIndex={0} 
                              key={title + index} 
                              className="group border-[0.5px] border-gray-400 rounded-xl p-6 cursor-pointer hover:bg-blue-50 active:bg-blue-100 focus:bg-blue-50 hover:ring-1 hover:ring-blue-300 hover:-translate-y-1 duration-500
                              hover:shadow-lg transition-colors"
                            >
                                <Image src={icon} alt={title} className="w-7 mt-3" width={28} height={28} />
                                <div>
                                    <h3 className="my-4 font-semibold text-gray-700 transition-colors group-hover:text-blue-600 group-active:text-blue-600 group-focus:text-blue-600 font-Ovo">{title}</h3>
                                    <p className="text-gray-600 text-sm transition-colors group-hover:text-gray-800 group-active:text-gray-800 group-focus:text-gray-800 font-Ovo">{description}</p>
                                </div>
                            </motion.li>
                        ))}
                    </motion.ul>
                    
                    <motion.h4 
                      initial={{ y: 20, opacity: 0 }}
                      whileInView={{ y: 0, opacity: 1 }}
                      transition={{ duration: 0.4, delay: 0.6 }}
                      viewport={{ once: true }}
                      className="my-6 text-gray-700 font-Ovo"
                    >
                        Tools I use
                    </motion.h4>
                    
                    <motion.ul 
                      initial={{ opacity: 0 }}
                      whileInView={{ opacity: 1 }}
                      transition={{ duration: 0.4, delay: 0.7 }}
                      viewport={{ once: true }}
                      className="flex items-center gap-3 sm:gap-5"
                    >
                        {toolsData.map((tool,index)=>{
                            // Utiliser la version sombre de Firebase en mode sombre
                            const toolSrc = (tool === assets.firebase && isDarkMode) ? assets.firebase_dark : tool;
                            return (
                                <motion.li 
                                  initial={{ opacity: 0 }}
                                  whileInView={{ opacity: 1 }}
                                  transition={{ duration: 0.3, delay: 0.8 + index * 0.05 }}
                                  viewport={{ once: true }}
                                  className="flex items-center justify-center w-12 sm:w-14 aspect-square border border-gray-400 rounded-lg cursor-pointer hover:-translate-y-1 duration-500"
                                  key={index}
                                >
                                    <Image src={toolSrc} alt="Tool" className='w-5 sm:w-7'/>
                                </motion.li>
                            );
                        })}
                    </motion.ul>
                </motion.div>
            </div>
        </motion.div>
    )
}